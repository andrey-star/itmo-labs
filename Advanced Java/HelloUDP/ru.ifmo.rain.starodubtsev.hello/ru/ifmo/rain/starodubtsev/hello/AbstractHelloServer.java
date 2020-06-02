package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.function.Supplier;

import static ru.ifmo.rain.starodubtsev.hello.Utils.getIntArg;

public abstract class AbstractHelloServer implements HelloServer {
	
	public static final Charset CHARSET = StandardCharsets.UTF_8;
	
	protected static void launch(String[] args, Supplier<HelloServer> serverSupplier) {
		Objects.requireNonNull(args);
		if (args.length != 2) {
			info("Usage: HelloServer <port> <threads>");
			return;
		}
		try {
			final int port = getIntArg(0, args);
			final int threads = getIntArg(1, args);
			try (final HelloServer server = serverSupplier.get()) {
				server.start(port, threads);
				try (BufferedReader in = new BufferedReader(new InputStreamReader(System.in))) {
					in.readLine();
				} catch (IOException e) {
					error(e, "Error when trying to listen to input");
				}
			}
		} catch (final NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	protected static void error(final Exception e, final String message) {
		Logger.error(e, "[Server]", message);
	}
	
	protected static void info(final String message) {
		Logger.info("[Server]", message);
	}
	
	
	
	protected String response(final String request) {
		return "Hello, " + request;
	}
	
}
