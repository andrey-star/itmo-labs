package ru.ifmo.rain.starodubtsev.implementor;

import java.io.PrintStream;

/**
 * A class capable of logging messages and errors.
 */
public class Logger {
	
	private static final PrintStream ERR_STREAM = System.err;
	private static final PrintStream OUT_STREAM = System.out;
	private static final boolean PRINT_STACK_TRACE = false;
	
	public static void error(final Exception e, final String... message) {
		if (PRINT_STACK_TRACE) {
			e.printStackTrace();
		}
		ERR_STREAM.println(e.getMessage() + " " + String.join(" ", message));
	}
	
	public static void info(final String... message) {
		OUT_STREAM.println(String.join(" ", message));
	}
	
	
}
