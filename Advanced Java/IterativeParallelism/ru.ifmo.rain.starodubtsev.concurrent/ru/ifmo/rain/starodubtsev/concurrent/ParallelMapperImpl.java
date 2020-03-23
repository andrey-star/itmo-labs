package ru.ifmo.rain.starodubtsev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Implementation of the {@code ParallelMapper} interface, capable of mapping values concurrently.
 *
 * @author Andrey Starodubtsev
 * @see ParallelMapper
 */
public class ParallelMapperImpl implements ParallelMapper {
	
	private static final int MAX_TASKS = 1_000_000;
	private final Queue<Runnable> tasks;
	private final List<Thread> threads;
	private final Set<FutureList<?>> active;
	private volatile boolean closed = false;
	
	/**
	 * Thread number constructor. Creates an instance of {@code ParallelMapperImpl}
	 * with the specified amount of {@code threads}.
	 *
	 * @param threads the amount of threads
	 */
	public ParallelMapperImpl(int threads) {
		active = new HashSet<>();
		tasks = new ArrayDeque<>();
		Runnable runner = () -> {
			try {
				while (!Thread.interrupted()) {
					runTask();
				}
			} catch (InterruptedException ignored) {
			} finally {
				Thread.currentThread().interrupt();
			}
		};
		this.threads = Stream.generate(() -> new Thread(runner))
		                     .limit(threads)
		                     .collect(Collectors.toList());
		this.threads.forEach(Thread::start);
	}
	
	private void runTask() throws InterruptedException {
		Runnable task;
		synchronized (tasks) {
			while (tasks.isEmpty()) {
				tasks.wait();
			}
			task = tasks.poll();
			tasks.notifyAll();
		}
		task.run();
	}
	
	private void addTask(Runnable task) throws InterruptedException {
		synchronized (tasks) {
			while (tasks.size() > MAX_TASKS) {
				tasks.wait();
			}
			tasks.add(task);
			tasks.notifyAll();
		}
	}
	
	/**
	 * Maps function {@code f} over specified {@code args}.
	 * Mapping for each element performs in parallel.
	 *
	 * @param f    the mapping function
	 * @param args the elements to be processed
	 * @param <T>  type of argument
	 * @param <R>  type of resulting mapped arguments
	 * @return a {@code List} of mapped arguments
	 * @throws InterruptedException if calling thread was interrupted
	 */
	@Override
	public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
		FutureList<R> futureList = new FutureList<>(args.size());
		synchronized (active) {
			active.add(futureList);
		}
		RuntimeException re = new RuntimeException("Runtime exception(s) occurred during execution");
		for (int i = 0; i < args.size(); i++) {
			T value = args.get(i);
			final int finalI = i;
			addTask(() -> {
				R mappedValue = null;
				try {
					mappedValue = f.apply(value);
				} catch (RuntimeException e) {
					synchronized (re) {
						re.addSuppressed(e);
					}
					futureList.finish();
				} finally {
					futureList.set(finalI, mappedValue);
				}
			});
		}
		List<R> res = futureList.get();
		if (re.getSuppressed().length != 0) {
			throw re;
		}
		synchronized (active) {
			active.remove(futureList);
		}
		return res;
	}
	
	/**
	 * Stops all threads. All unfinished mappings leave in undefined state.
	 */
	@Override
	public void close() {
		closed = true;
		synchronized (active) {
			active.forEach(FutureList::finish);
		}
		threads.forEach(Thread::interrupt);
		for (int i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (InterruptedException e) {
				i--; // have to wait for current thread to finish
			}
		}
	}
	
	private class FutureList<T> {
		
		private final List<T> result;
		boolean finished = false;
		private int set = 0;
		
		private FutureList(int size) {
			result = new ArrayList<>(Collections.nCopies(size, null));
		}
		
		public synchronized void set(int index, T value) {
			result.set(index, value);
			set++;
			if (set == result.size()) {
				finish();
			}
		}
		
		public synchronized List<T> get() throws InterruptedException {
			while (!finished && !closed) {
				wait();
			}
			return result;
		}
		
		public synchronized void finish() {
			finished = true;
			notify();
		}
	}
}
