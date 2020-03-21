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
		this.tasks