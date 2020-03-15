package ru.ifmo.rain.starodubtsev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Implementation of the {@code ListIP} interface, providing methods for parallel data processing.
 *
 * @author Andrey Starodubtsev
 * @see ListIP
 * @see info.kgeorgiy.java.advanced.concurrent.ScalarIP
 */
@SuppressWarnings("OptionalGetWithoutIsPresent") // ok to throw NoSuchElementException
public class IterativeParallelism implements ListIP {
	
	@Override
	public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
		Function<Stream<? extends T>, ? extends T> min = stream -> stream.min(comparator).get();
		return parallelReduce(threads, values, min, min);
	}
	
	@Override
	public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
		return minimum(threads, values, Collections.reverseOrder(comparator));
	}
	
	@Override
	public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.allMatch(predicate),
				stream -> stream.allMatch(Boolean::booleanValue));
	}
	
	@Override
	public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.anyMatch(predicate),
				stream -> stream.anyMatch(Boolean::booleanValue));
	}
	
	@Override
	public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.filter(predicate).collect(Collectors.toList()),
				this::collectToList);
	}
	
	@Override
	public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.map(f).collect(Collectors.toList()),
				this::collectToList);
	}
	
	@Override
	public String join(int threads, List<?> values) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.map(Object::toString).collect(Collectors.joining()),
				stream -> stream.collect(Collectors.joining()));
	}
	
	/**
	 * Reduces the given values by provided reducing functions. Concurrently processes the provided
	 * {@code values} by splitting them into several chunks, processing each in a separate {@code Thread},
	 * and combining the result.
	 *
	 * @param threadsCount number of concurrent threads
	 * @param values       values to reduce
	 * @param threadReduce a {@code Function}, capable of reducing a {@code Stream} of values of type {@code T}
	 *                     to a single value of type {@code U}
	 * @param resReduce    a {@code Function}, capable of reducing a {@code Stream} of values of type {@code U}
	 *                     to a single value of type {@code U}
	 * @param <T>          type of element in {@code values}
	 * @param <R>          result type
	 * @return the result of reducing
	 * @throws InterruptedException if executing thread was interrupted
	 * @see Thread
	 */
	public <T, R> R parallelReduce(int threadsCount, List<? extends T> values,
	                               Function<Stream<? extends T>, ? extends R> threadReduce,
	                               Function<Stream<? extends R>, ? extends R> resReduce) throws InterruptedException {
		List<Stream<? extends T>> chunks = split(values, threadsCount);
		List<Thread> threads = new ArrayList<>();
		List<R> res = new ArrayList<>(Collections.nCopies(chunks.size(), null));
		for (int i = 0; i < chunks.size(); i++) {
			final int index = i;
			Thread thread = new Thread(() -> res.set(index, threadReduce.apply(chunks.get(index))));
			threads.add(thread);
			thread.start();
		}
		joinThreads(threads);
		return resReduce.apply(res.stream());
	}
	
	/**
	 * Splits the given {@code values} into chunks. Returns a {@code List} with {@code blocks} amount of chunks,
	 * or less, if {@code blocks} is greater than the amount elements in {@code values}.
	 *
	 * @param values values to split
	 * @param blocks amount of resulting blocks
	 * @param <T>    type of element in the {@code List}
	 * @return a {@code List} of chunks, each one represented by a {@code Stream} of values
	 */
	private <T> List<Stream<? extends T>> split(List<? extends T> values, int blocks) {
		int blockSize = values.size() / blocks;
		int rest = values.size() % blocks;
		
		List<Stream<? extends T>> chunks = new ArrayList<>();
		int start = 0;
		for (int i = 0; i < blocks; i++) {
			int end = start + blockSize;
			if (i < rest) {
				end++;
			}
			if (start < end) {
				chunks.add(values.subList(start, end).stream());
			}
			start = end;
		}
		return chunks;
	}
	
	/**
	 * Collects the provided {@code Stream} of {@code List}s into a single {@code List}.
	 *
	 * @param stream the {@code Stream} of {@code List}s
	 * @param <T> the type of element in {@code List}
	 * @return the joined {@code List}
	 * @see Stream#flatMap(Function)
	 */
	public <T> List<T> collectToList(Stream<? extends List<T>> stream) {
		return stream.flatMap(List::stream).collect(Collectors.toList());
	}
	
	/**
	 * Waits for all {@code Thread}s in {@code threads} to die.
	 *
	 * @param threads the threads to wait for
	 * @throws InterruptedException if executing thread was interrupted
	 * @see Thread#join()
	 */
	private void joinThreads(List<Thread> threads) throws InterruptedException {
		for (Thread thread : threads) {
			thread.join();
		}
	}
}
