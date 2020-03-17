package ru.ifmo.rain.starodubtsev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
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
@SuppressWarnings("OptionalGetWithoutIsPresent") // ok to throw NoSuchElementException.
public class IterativeParallelism implements AdvancedIP {
	
	/**
	 * Returns minimum value.
	 *
	 * @param threads    number or concurrent threads
	 * @param values     values to get minimum of
	 * @param comparator value comparator
	 * @param <T>        value type
	 * @return minimum of given values
	 * @throws InterruptedException             if executing thread was interrupted
	 * @throws java.util.NoSuchElementException if no values are given
	 * @see #min(Stream, Comparator)
	 * @see #parallelReduce(int, List, Function, Function)
	 */
	@Override
	public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> min(stream, comparator),
				stream -> min(stream, comparator));
	}
	
	
	/**
	 * Returns maximum value.
	 *
	 * @param threads    number or concurrent threads
	 * @param values     values to get maximum of
	 * @param comparator value comparator
	 * @param <T>        value type
	 * @return maximum of given values
	 * @throws InterruptedException             if executing thread was interrupted
	 * @throws java.util.NoSuchElementException if no values are given
	 * @see #parallelReduce(int, List, Function, Function)
	 * @see #minimum(int, List, Comparator)
	 * @see Collections#reverseOrder(Comparator)
	 */
	@Override
	public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
		return minimum(threads, values, Collections.reverseOrder(comparator));
	}
	
	/**
	 * Returns min value of {@code stream} by provided {@code comparator}.
	 *
	 * @param stream     the stream of values
	 * @param comparator the comparator
	 * @param <T>        value type
	 * @return minimum value
	 * @throws NoSuchElementException if no values are given
	 * @see Stream#min(Comparator)
	 */
	private <T> T min(Stream<T> stream, Comparator<? super T> comparator) {
		return stream.min(comparator).get();
	}
	
	/**
	 * Returns whether all values satisfy predicate.
	 *
	 * @param threads   number or concurrent threads
	 * @param values    values to test
	 * @param predicate test predicate
	 * @param <T>       value type
	 * @return whether all values satisfies predicate or {@code true}, if no values are given
	 * @throws InterruptedException if executing thread was interrupted
	 * @see #parallelReduce(int, List, Function, Function)
	 * @see Stream#allMatch(Predicate)
	 */
	@Override
	public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.allMatch(predicate),
				stream -> stream.allMatch(Boolean::booleanValue));
	}
	
	/**
	 * Returns whether any value satisfies predicate.
	 *
	 * @param threads   number or concurrent threads
	 * @param values    values to test
	 * @param predicate test predicate
	 * @param <T>       value type
	 * @return whether any value satisfies predicate or {@code false}, if no values are given
	 * @throws InterruptedException if executing thread was interrupted
	 * @see #parallelReduce(int, List, Function, Function)
	 * @see #all(int, List, Predicate)
	 */
	@Override
	public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
		return !all(threads, values, predicate.negate());
	}
	
	/**
	 * Filters values by predicate.
	 *
	 * @param threads   number of concurrent threads
	 * @param values    values to filter
	 * @param predicate filter predicate
	 * @param <T>       value type
	 * @return list of values satisfying given predicated. Order of values is preserved
	 * @throws InterruptedException if executing thread was interrupted
	 * @see #parallelReduce(int, List, Function, Function)
	 * @see Stream#filter(Predicate)
	 * @see #collectToList(Stream)
	 */
	@Override
	public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.filter(predicate).collect(Collectors.toList()),
				this::collectToList);
	}
	
	/**
	 * Maps values.
	 *
	 * @param threads number of concurrent threads
	 * @param values  values to filter
	 * @param f       mapper function
	 * @param <T>     value type
	 * @return list of values mapped by given function
	 * @throws InterruptedException if executing thread was interrupted
	 * @see #parallelReduce(int, List, Function, Function)
	 * @see Stream#map(Function)
	 * @see #collectToList(Stream)
	 */
	@Override
	public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.map(f).collect(Collectors.toList()),
				this::collectToList);
	}
	
	/**
	 * Join values to string.
	 *
	 * @param threads number of concurrent threads
	 * @param values  values to join
	 * @return list of joined result of {@link #toString()} call on each value
	 * @throws InterruptedException if executing thread was interrupted
	 * @see #parallelReduce(int, List, Function, Function)
	 * @see Stream#map(Function)
	 * @see Collectors#joining()
	 */
	@Override
	public String join(int threads, List<?> values) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.map(Object::toString).collect(Collectors.joining()),
				stream -> stream.collect(Collectors.joining()));
	}
	
	/**
	 * Reduces values using monoid.
	 *
	 * @param threads number of concurrent threads
	 * @param values  values to reduce
	 * @param monoid  monoid to use
	 * @return values reduced by provided monoid or {@link Monoid#getIdentity() identity} if not values specified
	 * @throws InterruptedException if executing thread was interrupted
	 */
	@Override
	public <T> T reduce(int threads, List<T> values, Monoid<T> monoid) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> getReduce(stream, monoid),
				stream -> getReduce(stream, monoid));
	}
	
	/**
	 * Maps and reduces values using monoid.
	 *
	 * @param threads number of concurrent threads
	 * @param values  values to reduce
	 * @param lift    mapping function
	 * @param monoid  monoid to use
	 * @return values reduced by provided monoid or {@link Monoid#getIdentity() identity} if not values specified
	 * @throws InterruptedException if executing thread was interrupted
	 */
	@Override
	public <T, R> R mapReduce(int threads, List<T> values, Function<T, R> lift, Monoid<R> monoid) throws InterruptedException {
		return parallelReduce(threads, values,
				stream -> stream.map(lift).reduce(monoid.getIdentity(), monoid.getOperator()),
				stream -> getReduce(stream, monoid));
	}
	
	/**
	 * Gets the reducing function for provided {@code monoid}.
	 *
	 * @param stream the stream to apply on
	 * @param monoid the monoid
	 * @param <T>    value type
	 * @return the reduce value
	 */
	private <T> T getReduce(Stream<T> stream, Monoid<T> monoid) {
		return stream.reduce(monoid.getIdentity(), monoid.getOperator());
	}
	
	/**
	 * Reduces the given values by provided reducing functions. Concurrently processes the provided
	 * {@code values} by splitting them into several chunks, processing each in a separate {@code Thread},
	 * and combining the result.
	 *
	 * @param threadsCount number of concurrent threads
	 * @param values       values to reduce
	 * @param threadReduce a {@code Function}, capable of reducing a {@code Stream} of values of type {@code T}
	 *                     to a single value of type {@code M}
	 * @param resReduce    a {@code Function}, capable of reducing a {@code Stream} of values of type {@code M}
	 *                     to a single value of type {@code R}
	 * @param <T>          type of element in {@code values}
	 * @param <M>          type of middle element
	 * @param <R>          result type
	 * @return the result of reducing
	 * @throws InterruptedException if executing thread was interrupted
	 * @see Thread
	 */
	public <T, M, R> R parallelReduce(int threadsCount, List<T> values,
	                                  Function<Stream<T>, M> threadReduce,
	                                  Function<Stream<M>, R> resReduce) throws InterruptedException {
		List<Stream<T>> chunks = split(values, threadsCount);
		List<Thread> threads = new ArrayList<>();
		List<M> res = new ArrayList<>(Collections.nCopies(chunks.size(), null));
		for (int i = 0; i < chunks.size(); i++) {
			final int finalI = i;
			Thread thread = new Thread(() -> res.set(finalI, threadReduce.apply(chunks.get(finalI))));
			threads.add(thread);
			thread.start();
		}
		joinThreads(threads);
		return resReduce.apply(res.stream());
	}
	
	private void joinThreads(List<Thread> threads) throws InterruptedException {
		InterruptedException ie = null;
		int i;
		for (i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (InterruptedException e) {
				ie = new InterruptedException("Could not join thread because executing thread was interrupted");
				ie.addSuppressed(e);
				break;
			}
		}
		if (ie != null) {
			for (int j = i; j < threads.size(); j++) {
				threads.get(j).interrupt();
			}
			for (int j = i; j < threads.size(); j++) {
				try {
					threads.get(j).join();
				} catch (InterruptedException e) {
					ie.addSuppressed(e);
					j--; // need to wait for current thread
				}
			}
			throw ie;
		}
	}
	
	/**
	 * Splits the given {@code values} into chunks. Returns a {@code List} with {@code blocks} amount of chunks,
	 * or less, if {@code blocks} is greater than the amount of elements in {@code values}.
	 *
	 * @param values values to split
	 * @param blocks amount of resulting chunks
	 * @param <T>    type of element in the {@code List}
	 * @return a {@code List} of chunks, each one represented by a {@code Stream} of values
	 * @see List#subList(int, int)
	 */
	private <T> List<Stream<T>> split(List<T> values, int blocks) {
		int blockSize = values.size() / blocks;
		int rest = values.size() % blocks;
		
		List<Stream<T>> chunks = new ArrayList<>();
		int start = 0;
		for (int i = 0; i < blocks; i++) {
			int end = start + blockSize;
			if (i < rest) {
				end++;
			}
			if (start == end) {
				break;
			}
			chunks.add(values.subList(start, end).stream());
			start = end;
		}
		return chunks;
	}
	
	/**
	 * Collects the provided {@code Stream} of {@code List}s into a single {@code List}.
	 *
	 * @param stream the {@code Stream} of {@code List}s
	 * @param <T>    the type of element in {@code List}
	 * @return the joined {@code List}
	 * @see Stream#flatMap(Function)
	 */
	public <T> List<T> collectToList(Stream<? extends List<? extends T>> stream) {
		return stream.flatMap(List::stream).collect(Collectors.toList());
	}
}
