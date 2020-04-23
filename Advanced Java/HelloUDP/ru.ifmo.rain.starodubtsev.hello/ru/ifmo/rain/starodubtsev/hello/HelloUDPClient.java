package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static ru.ifmo.rain.starodubtsev.hello.MainUtils.getArg;
import static ru.ifmo.rain.starodubtsev.hello.MainUtils.getIntArg;

/**
 * An implementation of the {@code HelloClient} interface.
 * Capable of sending requests to a server via UDP.
 *
 * @see HelloClient
 */
public class HelloUDPClient implements HelloClient {
	
	public static final long AWAIT_TERMINATION_MILLISECONDS = Long.MAX_VALUE;
	private static final int SO_TIMEOUT = 100;
	
	/**
	 * Main method. A command line utility for {@code HelloUDPClient}.
	 * Usage: {@code HelloUDPClient <host> <port> <prefix> <threads> <requests>}.
	 * Creates and runs the {@code client} with the specified parameters.
	 *
	 * @param args program arguments
	 * @see #run(String, int, String, int, int)
	 */
	public static void main(final String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 5) {
			info("Usage: HelloUDPClient <host> <port> <prefix> <threads> <requests>");
			return;
		}
		try {
			final String host = getArg(0, args);
			final int port = getIntArg(1, args);
			final String prefix = getArg(2, args);
			final int threads = getIntArg(3, args);
			final int requests = getIntArg(4, args);
			new HelloUDPClient().run(host, port, prefix, threads, requests);
		} catch (final NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	private static void error(final Exception e, final String message) {
		Logger.error(e, "[Client]", message);
	}
	
	private static void info(final String message) {
		Logger.info("[Client]", message);
	}
	
	@Override
	public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
		final SocketAddress socketAddress = new InetSocketAddress(host, port);
		final ExecutorService requestPool = Executors.newFixedThreadPool(threads);
		for (int threadId = 0; threadId < threads; threadId++) {
			final int finalThreadId = threadId;
			requestPool.submit(() -> processThread(prefix, finalThreadId, requests, socketAddress));
		}
		await(requestPool);
	}
	
	private void await(final ExecutorService executorService) {
		while (true) {
			executorService.shutdown();
			try {
				executorService.awaitTermination(AWAIT_TERMINATION_MILLISECONDS, TimeUnit.MILLISECONDS);
				break;
			} catch (final InterruptedException e) {
				error(e, "Executing thread was interrupted while waiting for termination");
			}
		}
	}
	
	private void processThread(final String prefix, final int threadId, final int requests, final SocketAddress address) {
		try (final DatagramSocket socket = new DatagramSocket()) {
			socket.setSoTimeout(SO_TIMEOUT);
			final DatagramPacket packet = new DatagramPacket(new byte[0], 0, address);
			final byte[] receive = new byte[socket.getReceiveBufferSize()];
			for (int requestId = 0; requestId < requests; requestId++) {
				final String request = getRequest(prefix, threadId, requestId);
				while (!socket.isClosed() && !Thread.interrupted()) {
					try {
						DatagramUtils.send(request, packet, socket);
						final String response = DatagramUtils.setDataAndReceive(receive, packet, socket);
						if (isResponseValid(response, threadId, requestId)) {
							info(response);
							break;
						} else {
							info("Received invalid response to request '" + request + "': " + response);
						}
					} catch (final IOException e) {
						error(e, "Error occurred during communication with the server");
					}
				}
			}
		} catch (final SocketException e) {
			error(e, "Error when creating socket");
		}
	}
	
	private boolean isResponseValid(final String response, final int threadId, final int requestId) {
		return new ResponseChecker().check(response, threadId, requestId);
	}
	
	private String getRequest(final String prefix, final int threadId, final int requestId) {
		return String.format("%s%s_%s", prefix, threadId, requestId);
	}
	
	private static class ResponseChecker {
		
		private int index;
		
		boolean check(String response, int threadId, int requestId) {
			if (!checkNumber(response, threadId) || !checkNumber(response, requestId)) {
				return false;
			}
			index = skipNonDigits(response, index);
			return index == response.length();
		}
		
		private boolean checkNumber(String response, int requestId) {
			index = skipNonDigits(response, index);
			int number = parseInt(response, index);
			if (number != requestId) {
				return false;
			}
			index += Integer.toString(number).length();
			return true;
		}
		
		private int skipNonDigits(final String response, int index) {
			while (index < response.length() && !Character.isDigit(response.charAt(index))) {
				index++;
			}
			return index;
		}
		
		private int parseInt(final String response, int index) {
			final StringBuilder sb = new StringBuilder();
			while (index < response.length() && Character.isDigit(response.charAt(index))) {
				sb.append(response.charAt(index++));
			}
			try {
				return Integer.parseInt(sb.toString());
			} catch (final NumberFormatException e) {
				return -1;
			}
		}
	}
}
