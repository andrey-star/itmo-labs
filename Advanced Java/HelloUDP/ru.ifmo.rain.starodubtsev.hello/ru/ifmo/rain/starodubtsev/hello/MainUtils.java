package ru.ifmo.rain.starodubtsev.hello;

import java.util.Objects;

public class MainUtils {
	
	public static String getArg(final int i, final String[] args) {
		return Objects.requireNonNull(args[i]);
	}
	
	public static int getIntArg(final int i, final String[] args) {
		return Integer.parseInt(getArg(i, args));
	}
	
}
