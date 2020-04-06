package ru.ifmo.rain.starodubtsev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of the {@code ParallelMapper} interface, capable of mapping values concurrently.
 *
 * @author Andrey Starodubtsev
 * @see ParallelMapper
 */
public class ParallelMapperImpl implements ParallelMapper {
	
	private final boolean PRINT_STACK_TRACE = false;
	
	private final TaskQueue tasks;
	private final List<Thread> threads;
	private volatile boolean closed = false;
	
	/**
	 * Thread number constructor. Creates an instance of {@code ParallelMapperImpl}
	 * with the specified amount of {@code threads}.
	 *
	 * @param threads the amount of threads
	 */
	public ParallelMapperImpl(final int threads) {
		tasks = new TaskQueue();
		final Runnable runner = () -> {
			try {
				while (!Thread.interrupted()) {
					tasks.nextSubTask().run();
				}
			} catch (final InterruptedException e) {
				if (PRINT_STACK_TRACE) {
					e.printStackTrace();
				}
			} finally {
				Thread.currentThread().interrupt();
			}
		};
		this.threads = Stream.generate(() -> new Thread(runner))
		                     .limit(threads)
		                     .collect(Collectors.toList());
		this.threads.forEach(Thread::start);
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
		if (args.isEmpty()) {
			return List.of();
		}
		final Task<T, R> task = new Task<>(f, args);
		tasks.add(task);
		return task.awaitResult();
	}
	
	/**
	 * Stops all threads. All unfinished mappings leave in undefined state.
	 */
	@Override
	public void close() {
		closed = true;
		threads.forEach(Thread::interrupt);
		tasks.forEach(Task::terminate);
		joinThreads();
	}
	
	private void joinThreads() {
		for (int i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (final InterruptedException e) {
				if (PRINT_STACK_TRACE) {
					e.printStackTrace();
				}
				i--; // have to wait for current thread to finish
			}
		}
	}
	
	private static class TaskQueue {
		
		private final Queue<Task<?, ?>> queue;
		
		public TaskQueue() {
			this.queue = new ArrayDeque<>();
		}
		
		public synchronized void add(final Task<?, ?> task) {
			queue.add(task);
			notifyAll();
		}
		
		public synchronized void forEach(final Consumer<? super Task<?, ?>> action) {
			queue.forEach(action);
		}
		
		public synchronized void remove() {
			queue.remove();
		}
		
		public synchronized Runnable nextSubTask() throws InterruptedException {
			while (queue.isEmpty()) {
				wait();
			}
			final Task<?, ?> task = queue.element();
			Runnable subTask = task.getSubTask();
			return () -> {
				subTask.run();
				task.subTaskFinished();
			};
		}
	}
	
	private class Task<T, R> {
		
		public final Queue<Runnable> subTasks;
		private final ConcurrentMappingList list;
		private volatile boolean terminated = false;
		private int notFinished;
		private int notStarted;
		
		public Task(final Function<? super T, ? extends R> f, final List<? extends T> args) {
			subTasks = new ArrayDeque<>();
			list = new ConcurrentMappingList(args.size(), f);
			notFinished = notStarted = args.size();
			IntStream.range(0, args.size())
			         .forEach(i -> subTasks.add(() -> list.set(i, args.get(i))));
		}
		
		public synchronized Runnable getSubTask() {
			Runnable subTask = subTasks.remove();
			notStarted--;
			if (notStarted == 0) {
				tasks.remove();
			}
			return subTask;
		}
		
		public synchronized void subTaskFinished() {
			notFinished--;
			if (notFinished == 0) {
				terminate();
			}
		}
		
		public synchronized List<R> awaitResult() throws InterruptedException {
			while (!terminated && !closed) {
				wait();
			}
			if (!terminated) {
				terminate();
			}
			return list.get();
		}
		
		public synchronized void terminate() {
			terminated = true;
			notify();
		}
		
		private class ConcurrentMappingList {
			
			private final List<R> result;
			private final Function<? super T, ? extends R> f;
			private RuntimeException re = null;
			
			public ConcurrentMappingList(final int size, final Function<? super T, ? extends R> f) {
				result = new ArrayList<>(Collections.nCopies(size, null));
				this.f = f;
			}
			
			public void set(final int index, final T value) {
				try {
					if (!terminated) {
						setValue(index, f.apply(value));
					}
				} catch (final RuntimeException e) {
					addException(e);
				}
			}
			
			private synchronized void addException(RuntimeException e) {
				if (!terminated) {
					if (re == null) {
						re = e;
					} else {
						re.addSuppressed(e);
					}
				}
			}
			
			private synchronized void setValue(int index, R value) {
				if (!terminated) {
					result.set(index, value);
				}
			}
			
			public synchronized List<R> get() {
				if (re != null) {
					throw re;
				}
				return result;
			}
		}
	}
}
