package ru.ifmo.rain.starodubtsev.walk.logger;

import java.io.PrintStream;

public class Logger {
	
	private static final PrintStream LOG_STREAM = System.out;
	private static final boolean PRINT_STACK_TRACE = false;
	
	public static void error(final Exception e, final String message) {
		if (PRINT_STACK_TRACE) {
			e.printStackTrace();
		}
		LOG_STREAM.println(message);
	}
	
}
