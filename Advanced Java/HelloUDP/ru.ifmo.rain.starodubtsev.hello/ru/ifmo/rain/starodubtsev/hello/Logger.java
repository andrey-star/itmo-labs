package ru.ifmo.rain.starodubtsev.hello;

import java.io.PrintStream;

/**
 * A class capable of logging messages and errors.
 *
 * @author Andrey Starodubtsev
 */
public class Logger {
	
	/**
	 * Stream for errors.
	 */
	private static final PrintStream ERR_STREAM = System.err;
	/**
	 * Stream for information messages.
	 */
	private static final PrintStream OUT_STREAM = System.out;
	/**
	 * Indicates whether the logger should print stack traces of occurring exceptions.
	 */
	private static final boolean PRINT_STACK_TRACE = true;
	/**
	 * If true - silences the logger.
	 */
	private static final boolean SILENT = false;
	
	/**
	 * Logs the provided error, printing the stack trace if necessary.
	 * Joins the message arguments by space.
	 *
	 * @param e       the exception
	 * @param message error message
	 */
	public static void error(final Exception e, final String... message) {
		if (!SILENT) {
			if (PRINT_STACK_TRACE) {
				e.printStackTrace();
			}
			String errorMessage = String.join(" ", message);
			if (!errorMessage.isBlank()) {
				errorMessage += ": ";
			}
			ERR_STREAM.println(errorMessage + e.getMessage());
		}
	}
	
	/**
	 * Logs the provided {@code message}. Joins the arguments by space.
	 *
	 * @param message the message
	 */
	public static void info(final String... message) {
		if (!SILENT) {
			OUT_STREAM.println(String.join(" ", message));
		}
	}
}
