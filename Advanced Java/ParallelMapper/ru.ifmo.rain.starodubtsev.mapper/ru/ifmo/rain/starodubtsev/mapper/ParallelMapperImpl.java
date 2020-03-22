package ru.ifmo.rain.starodubtsev.mapper;

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
	private final List<Worker> workers;
	
	/**
	 * Thread count constructor. Creates an instance of {@code ParallelMapperImpl}
	 * with the specified amount of {@code threads}.
	 *
	 * @param threads the amount of threads
	 */
	public ParallelMapperImpl(int threads) {
		tasks = new ArrayDeque<>();
		workers = Stream.generate(Worker::new)
		                .limit(threads)
		                .collect(Collectors.toList());
		workers.forEach(Worker::start);
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
		for (int i = 0; i < args.size(); i++) {
			T value = args.get(i);
			final int finalI = i;
			addTask(() -> {
				R mappedValue = null;
				try {
					mappedValue = f.apply(value);
				} finally {
					futureList.set(finalI, mappedValue);
				}
			});
		}
		return futureList.get();
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
		workers.forEach(Thread::interrupt);
		for (int i = 0; i < workers.size(); i++) {
			try {
				workers.get(i).join();
			} catch (InterruptedException e) {
				i--; // have to wait for current thread to finish
			}
		}
	}
	
	private static class FutureList<R> {
		
		private final List<R> result;
		private int set = 0;
		
		private FutureList(int size) {
			result = new ArrayList<>(Collections.nCopies(size, null));
		}
		
		public synchronized void set(int index, R value) {
			result.set(index, value);
			set++;
			if (set == result.size()) {
				notify();
			}
		}
		
		public synchronized List<R> get() throws InterruptedException {
			while (set != result.size()) {
				wait();
			}
			return result;
		}
	}
	
	private class Worker extends Thread {
		@Override
		public void run() {
			try {
				while (!Thread.interrupted()) {
					runTask();
				}
			} catch (InterruptedException ignored) {
			} finally {
				Thread.currentThread().interrupt();
			}
		}
		
		@Override
		public UncaughtExceptionHandler getUncaughtExceptionHandler() {
			return (t, e) -> System.out.println("An exception occurred in thread " + t + ". " + e);
		}
	}
}
