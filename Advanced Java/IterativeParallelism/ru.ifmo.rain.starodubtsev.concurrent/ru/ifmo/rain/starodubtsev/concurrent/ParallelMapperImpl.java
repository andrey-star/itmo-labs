package ru.ifmo.rain.starodubtsev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Consumer;
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
	
	private final SynchronizedQueue<Task<?, ?>> tasks;
	private final List<Thread> threads;
	
	/**
	 * Thread number constructor. Creates an instance of {@code ParallelMapperImpl}
	 * with the specified amount of {@code threads}.
	 *
	 * @param threads the amount of threads
	 */
	public ParallelMapperImpl(final int threads) {
		tasks = new SynchronizedQueue<>();
		final Runnable runner = () -> {
			try {
				while (!Thread.interrupted()) {
					runTask();
				}
			} catch (final InterruptedException e) {
//				e.printStackTrace();
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
		final var task = tasks.element();
		final Runnable subTask = task.getSubTask();
		if (subTask != null) {
			subTask.run();
			task.subTaskFinished();
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
	public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
		final Task<T, R> task = new Task<>(f, args.size());
		tasks.add(task);
		task.createSubTasks(args);
		task.await();
		handleExceptions(task.getRuntimeExceptions());
		return task.getList();
	}
	
	private void handleExceptions(final List<RuntimeException> runtimeExceptions) {
		if (!runtimeExceptions.isEmpty()) {
			final var re = runtimeExceptions.get(0);
			runtimeExceptions.subList(1, runtimeExceptions.size()).forEach(re::addSuppressed);
			throw re;
		}
	}
	
	/**
	 * Stops all threads. All unfinished mappings leave in undefined state.
	 */
	@Override
	public void close() {
		threads.forEach(Thread::interrupt);
		tasks.forEach(Task::close);
		for (int i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (final InterruptedException e) {
				i--; // have to wait for current thread to finish
			}
		}
	}
	
	private static class SynchronizedQueue<T> {
		
		private final Queue<T> queue;
		
		public SynchronizedQueue() {
			this.queue = new ArrayDeque<>();
		}
		
		public synchronized T element() throws InterruptedException {
			while (queue.isEmpty()) {
				wait();
			}
			return queue.element();
		}
		
		public synchronized void add(final T task) {
			queue.add(task);
			notifyAll();
		}
		
		public synchronized void forEach(final Consumer<? super T> action) {
			queue.forEach(action);
		}
		
		public synchronized void remove() {
			queue.remove();
		}
	}
	
	private class Task<T, R> {
		
		private final Queue<Runnable> subTasks;
		private final ConcurrentMappingList list;
		private boolean closed = false;
		private int notFinished;
		private int notStarted;
		
		public Task(final Function<? super T, ? extends R> f, final int size) {
			subTasks = new ArrayDeque<>();
			list = new ConcurrentMappingList(size, f);
			notFinished = notStarted = size;
			if (notFinished == 0) {
				close();
			}
		}
		
		public synchronized void createSubTasks(final List<? extends T> args) {
			for (int i = 0; i < args.size(); i++) {
				addSubTask(args.get(i), i);
			}
		}
		
		private synchronized void addSubTask(final T value, final int index) {
			subTasks.add(() -> list.set(index, value));
		}
		
		public Runnable getSubTask() {
			Runnable subTask = null;
			if (!subTasks.isEmpty()) {
				synchronized (this) {
					if (!subTasks.isEmpty()) {
						subTask = subTasks.remove();
						notStarted--;
						if (notStarted == 0) {
							tasks.remove();
						}
					}
				}
			}
			return subTask;
		}
		
		public synchronized void subTaskFinished() {
			notFinished--;
			if (notFinished == 0) {
				close();
			}
		}
		
		public synchronized void await() throws InterruptedException {
			while (!closed) {
				wait();
			}
		}
		
		public synchronized void close() {
			list.close();
			subTasks.clear();
			closed = true;
			notify();
		}
		
		public synchronized List<R> getList() {
			return list.get();
		}
		
		public synchronized List<RuntimeException> getRuntimeExceptions() {
			return list.getRuntimeExceptions();
		}
		
		private class ConcurrentMappingList {
			
			private final List<R> result;
			private final Function<? super T, ? extends R> f;
			private final List<RuntimeException> runtimeExceptions = new ArrayList<>();
			private volatile boolean closed = false;
			
			public ConcurrentMappingList(final int size, final Function<? super T, ? extends R> f) {
				result = new ArrayList<>(Collections.nCopies(size, null));
				this.f = f;
			}
			
			public void set(final int index, final T value) {
				R apply = null;
				try {
					if (!closed) {
						apply = f.apply(value);
					}
				} catch (final RuntimeException e) {
					synchronized (this) {
						runtimeExceptions.add(e);
					}
				}
				if (!closed) {
					synchronized (this) {
						if (!closed) {
							result.set(index, apply);
						}
					}
				}
			}
			
			public synchronized List<R> get() {
				return result;
			}
			
			public synchronized List<RuntimeException> getRuntimeExceptions() {
				return runtimeExceptions;
			}
			
			public void close() {
				closed = true;
			}
		}
		
	}
}
