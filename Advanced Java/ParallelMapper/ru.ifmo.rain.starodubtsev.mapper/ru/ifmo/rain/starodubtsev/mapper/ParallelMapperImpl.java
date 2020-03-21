package ru.ifmo.rain.starodubtsev.mapper;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

/**
 * Implementation of the {@code ParallelMapper} interface. Capable of mapping values concurrently.
 *
 * @author Andrey Starodubtsev
 * @see ParallelMapper
 */
public class ParallelMapperImpl implements ParallelMapper {
	
	private static final int MAX_TASKS = 1_000_000;
	private final Queue<Runnable> tasks;
	private final List<Thread> threads;
	
	/**
	 * Thread count constructor. Creates an instance of {@code ParallelMapperImpl}
	 * with the specified amount of {@code threads}.
	 *
	 * @param threads the amount of threads
	 */
	public ParallelMapperImpl(int threads) {
		this.threads = new ArrayList<>();
		this.tasks = new ArrayDeque<>();
		for (int i = 0; i < threads; i++) {
			Runnable baseTask = baseTask();
			this.threads.add(new Thread(baseTask));
		}
		this.threads.forEach(Thread::start);
	}
	
	private Runnable baseTask() {
		return () -> {
			try {
				while (!Thread.interrupted()) {
					runTask();
				}
			} catch (InterruptedException ignored) {
			} finally {
				Thread.currentThread().interrupt();
			}
		};
	}
	
	
	/**
	 * Maps function {@code f} over specified {@code args}.
	 * Mapping for each element performs in parallel.
	 *
	 * @param f    the mapping function
	 * @param args the elements to be process
	 * @param <T>  type of argument
	 * @param <R>  type of resulting mapped arguments
	 * @return a {@code List} of mapped arguments
	 * @throws InterruptedException if calling thread was interrupted
	 */
	@Override
	public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
		FutureList<R> futureList = new FutureList<>(args.size());
		List<RuntimeException> exceptions = new ArrayList<>();
		for (int i = 0; i < args.size(); i++) {
			T value = args.get(i);
			final int finalI = i;
			addTask(() -> {
				R mappedValue = null;
				try {
					mappedValue = f.apply(value);
				} catch (RuntimeException e) {
					synchronized (exceptions) {
						exceptions.add(e);
					}
				}
				futureList.set(finalI, mappedValue);
			});
		}
		List<R> res = futureList.getList();
		if (!exceptions.isEmpty()) {
			exceptions.forEach(Exception::printStackTrace);
			return null;
		}
		return res;
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
	
	/**
	 * Stops all threads. All unfinished mappings leave in undefined state.
	 */
	@Override
	public void close() {
		threads.forEach(Thread::interrupt);
		for (int i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (InterruptedException e) {
				i--; // have to wait for current thread to finish
			}
		}
	}
	
	private static class FutureList<R> {
		
		private final List<R> result;
		private int set = 0;
		
		private FutureList(int size) {
			this.result = new ArrayList<>(Collections.nCopies(size, null));
		}
		
		public synchronized void set(int index, R value) {
			result.set(index, value);
			set++;
			if (set == result.size()) {
				notify();
			}
		}
		
		public synchronized List<R> getList() throws InterruptedException {
			while (set != result.size()) {
				wait();
			}
			return result;
		}
	}
}
