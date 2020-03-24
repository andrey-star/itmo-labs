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
	
	private final Queue<Runnable> tasks;
	private final List<Thread> threads;
	private boolean closed = false;
	
	/**
	 * Thread number constructor. Creates an instance of {@code ParallelMapperImpl}
	 * with the specified amount of {@code threads}.
	 *
	 * @param threads the amount of threads
	 */
	public ParallelMapperImpl(int threads) {
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
		}
		task.run();
	}
	
	private void addTask(Runnable task) {
		synchronized (tasks) {
			tasks.add(task);
			tasks.notify();
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
		FutureList<T, R> futureList = new FutureList<>(args.size(), f);
		for (int i = 0; i < args.size(); i++) {
			T value = args.get(i);
			final int finalI = i;
			addTask(() -> futureList.set(finalI, value));
		}
		List<R> res = futureList.get();
		processExceptions(futureList);
		return res;
	}
	
	private void processExceptions(FutureList<?, ?> futureList) {
		List<RuntimeException> runtimeExceptions = futureList.getRuntimeExceptions();
		if (!runtimeExceptions.isEmpty()) {
			RuntimeException re = new RuntimeException("Runtime exception(s) occurred during execution");
			runtimeExceptions.forEach(re::addSuppressed);
			throw re;
		}
	}
	
	/**
	 * Stops all threads. All unfinished mappings leave in undefined state.
	 */
	@Override
	public void close() {
		closed = true;
		threads.forEach(Thread::interrupt);
		for (int i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (InterruptedException e) {
				i--; // have to wait for current thread to finish
			}
		}
	}
	
	private class FutureList<T, R> {
		
		private final List<R> result;
		private final Function<? super T, ? extends R> f;
		private final List<RuntimeException> runtimeExceptions;
		boolean finished = false;
		private int set = 0;
		
		public FutureList(int size, Function<? super T, ? extends R> f) {
			result = new ArrayList<>(Collections.nCopies(size, null));
			this.f = f;
			runtimeExceptions = new ArrayList<>();
		}
		
		public void set(int index, T value) {
			R apply = null;
			try {
				apply = f.apply(value);
			} catch (RuntimeException e) {
				synchronized (runtimeExceptions) {
					runtimeExceptions.add(e);
				}
			} finally {
				synchronized (this) {
					set++;
					result.set(index, apply);
					if (set == result.size()) {
						finish();
					}
				}
			}
		}
		
		public synchronized List<R> get() throws InterruptedException {
			while (!finished && !closed) {
				wait();
			}
			return result;
		}
		
		public synchronized void finish() {
			if (finished) {
				return;
			}
			finished = true;
			notify();
		}
		
		public List<RuntimeException> getRuntimeExceptions() {
			return runtimeExceptions;
		}
	}
}
