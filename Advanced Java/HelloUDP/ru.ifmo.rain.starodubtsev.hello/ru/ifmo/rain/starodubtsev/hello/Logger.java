package ru.ifmo.rain.starodubtsev.hello;

import java.io.PrintStream;

public class Logger {
	
	private static final PrintStream ERR_STREAM = System.err;
	private static final PrintStream OUT_STREAM = System.out;
	private static final boolean PRINT_STACK_TRACE = false;
	
	public static void error(Exception e, String message) {
		if (PRINT_STACK_TRACE) {
			e.printStackTrace();
		}
		ERR_STREAM.println(message);
	}
	
	public static void log(String message) {
		OUT_STREAM.println(message);
	}
	
	
}
