package ru.ifmo.rain.starodubtsev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Implementation of the {@code AdvancedIP} interface, providing methods for parallel data processing.
 *
 * @author Andrey Starodubtsev
 * @see AdvancedIP
 * @see info.kgeorgiy.java.advanced.concurrent.ListIP
 * @see info.kgeorgiy.java.advanced.concurrent.ScalarIP
 * @see ParallelMapper
 */
@SuppressWarnings("OptionalGetWithoutIsPresent") // ok to throw NoSuchElementException
public class IterativeParallelism implements AdvancedIP {
	
	private final ParallelMapper mapper;
	
	/**
	 * Default constructor. Creates an instance of {@code IterativeParallelism}, doesn't use {@code ParallelMapper}.
	 */
	public IterativeParallelism() {
		this.mapper = null;
	}
	
	/**
	 * Mapper constructor. Creates an instance of {@code IterativeParallelism}
	 * with provided {@code ParallelMapper}, which is used for parallel processing.
	 *
	 * @param mapper the {@code ParallelMapper}
	 */
	public IterativeParallelism(final ParallelMapper mapper) {
		this.mapper = mapper;
	}
	
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
	public <T> T minimum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
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
	public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
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
	private <T> T min(final Stream<T> stream, final Comparator<? super T> comparator) {
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
	public <T> boolean all(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
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
	public <T> boolean any(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
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
	public <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
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
	public <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> f) throws InterruptedException {
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
	public String join(final int threads, final List<?> values) throws InterruptedException {
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
	public <T> T reduce(final int threads, final List<T> values, final Monoid<T> monoid) throws InterruptedException {
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
	public <T, R> R mapReduce(final int threads, final List<T> values, final Function<T, R> lift, final Monoid<R> monoid) throws InterruptedException {
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
	private <T> T getReduce(final Stream<T> stream, final Monoid<T> monoid) {
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
	public <T, M, R> R parallelReduce(final int threadsCount, final List<T> values,
	                                  final Function<Stream<T>, M> threadReduce,
	                                  final Function<Stream<M>, R> resReduce) throws InterruptedException {
		final List<Stream<T>> chunks = split(values, threadsCount);
		final List<M> res;
		if (mapper == null) {
			final List<Thread> threads = new ArrayList<>();
			res = new ArrayList<>(Collections.nCopies(chunks.size(), null));
			for (int i = 0; i < chunks.size(); i++) {
				final int finalI = i;
				final Thread thread = new Thread(() -> res.set(finalI, threadReduce.apply(chunks.get(finalI))));
				threads.add(thread);
				thread.start();
			}
			joinThreads(threads);
		} else {
			res = mapper.map(threadReduce, chunks);
		}
		return resReduce.apply(res.stream());
	}
	
	private void joinThreads(final List<Thread> threads) throws InterruptedException {
		InterruptedException ie = null;
		for (int i = 0; i < threads.size(); i++) {
			try {
				threads.get(i).join();
			} catch (final InterruptedException e) {
				if (ie == null) {
					ie = new InterruptedException("Could not join thread because executing thread was interrupted");
					for (int j = i; j < threads.size(); j++) {
						threads.get(j).interrupt();
					}
				}
				ie.addSuppressed(e);
				i--;
			}
		}
		if (ie != null) {
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
	private <T> List<Stream<T>> split(final List<T> values, final int blocks) {
		final int blockSize = values.size() / blocks;
		final int rest = values.size() % blocks;
		
		final List<Stream<T>> chunks = new ArrayList<>();
		int start = 0;
		for (int i = 0; i < blocks; i++) {
			int end = start + blockSize;
			if (i < rest) {
				end++;
			}
			if (start == end) { // no empty chunks are processed
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
	public <T> List<T> collectToList(final Stream<? extends List<? extends T>> stream) {
		return stream.flatMap(List::stream).collect(Collectors.toList());
	}
}
