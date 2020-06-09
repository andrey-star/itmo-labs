package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.util.Objects;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static ru.ifmo.rain.starodubtsev.hello.Utils.getArg;
import static ru.ifmo.rain.starodubtsev.hello.Utils.getIntArg;

public abstract class AbstractHelloClient implements HelloClient {
	
	public static final int SO_TIMEOUT = 100;
	
	private final Pattern RESPONSE_PATTERN = Pattern.compile("[\\D]*([\\d]+)[\\D]*([\\d]+)[\\D]*");
	
	public static void run(final String[] args, Supplier<HelloClient> clientSupplier) {
		Objects.requireNonNull(args);
		if (args.length != 5) {
			info("Usage: HelloClient <host> <port> <prefix> <threads> <requests>");
			return;
		}
		try {
			final String host = getArg(0, args);
			final int port = getIntArg(1, args);
			final String prefix = getArg(2, args);
			final int threads = getIntArg(3, args);
			final int requests = getIntArg(4, args);
			clientSupplier.get().run(host, port, prefix, threads, requests);
		} catch (final NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	protected static void error(final Exception e, final String message) {
		Logger.error(e, "[Client]", message);
	}
	
	protected static void info(final String message) {
		Logger.info("[Client]", message);
	}
	
	protected boolean isResponseValid(final String response, final int threadId, final int requestId) {
		Matcher matcher = RESPONSE_PATTERN.matcher(response);
		if (!matcher.find()) {
			return false;
		}
		return Integer.toString(threadId).equals(matcher.group(1))
				&& Integer.toString(requestId).equals(matcher.group(2));
	}
	
	protected String getRequest(final String prefix, final int threadId, final int requestId) {
		return String.format("%s%s_%s", prefix, threadId, requestId);
	}
	
}
