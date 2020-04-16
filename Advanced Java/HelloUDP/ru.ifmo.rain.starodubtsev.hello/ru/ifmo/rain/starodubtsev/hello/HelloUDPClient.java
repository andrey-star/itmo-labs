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
	
	private static final int SO_TIMEOUT = 100;
	
	/**
	 * Main method. Usage: {@code HelloUDPClient <host> <port> <prefix> <threads> <requests>}.
	 * Creates and runs the {@code client} with the specified parameters.
	 *
	 * @param args program arguments
	 * @see #run(String, int, String, int, int)
	 */
	public static void main(String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 5) {
			System.out.println("Usage: HelloUDPClient <host> <port> <prefix> <threads> <requests>");
			return;
		}
		try {
			final String host = getArg(0, args);
			final int port = getIntArg(1, args);
			final String prefix = getArg(2, args);
			final int threads = getIntArg(3, args);
			final int requests = getIntArg(4, args);
			new HelloUDPClient().run(host, port, prefix, threads, requests);
		} catch (NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	private static void error(Exception e, String message) {
		Logger.error(e, "[Client]", message);
	}
	
	private static void log(String message) {
		Logger.log("[Client]", message);
	}
	
	@Override
	public void run(String host, int port, String prefix, int threads, int requests) {
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
				executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
				break;
			} catch (InterruptedException e) {
				error(e, "Executing thread was interrupted while waiting for termination");
			}
		}
	}
	
	private void processThread(final String prefix, final int threadId, final int requests, final SocketAddress address) {
		try (final DatagramSocket socket = new DatagramSocket()) {
			socket.setSoTimeout(SO_TIMEOUT);
			DatagramPacket packet = new DatagramPacket(new byte[0], 0);
			for (int requestId = 0; requestId < requests; requestId++) {
				final String request = getRequest(prefix, threadId, requestId);
				while (!socket.isClosed() && !Thread.interrupted()) {
					try {
						final String response = DatagramUtils.request(request, packet, socket, address);
						if (isResponseValid(response, threadId, requestId)) {
							log(response);
							break;
						} else {
							log("Received invalid response to request '" + request + "': " + response);
						}
					} catch (IOException e) {
						error(e, "Error occurred during communication with the server");
					}
				}
			}
		} catch (SocketException e) {
			error(e, "Error when creating socket");
		}
	}
	
	private boolean isResponseValid(final String response, final int threadId, final int requestId) {
		int index = 0;
		index = skipNonDigits(response, index);
		int number = parseInt(response, index);
		if (number != threadId) {
			return false;
		}
		index += Integer.toString(number).length();
		index = skipNonDigits(response, index);
		number = parseInt(response, index);
		if (number != requestId) {
			return false;
		}
		index += Integer.toString(number).length();
		index = skipNonDigits(response, index);
		return index == response.length();
	}
	
	private int skipNonDigits(String response, int index) {
		while (index < response.length() && !Character.isDigit(response.charAt(index))) {
			index++;
		}
		return index;
	}
	
	private int parseInt(String response, int index) {
		StringBuilder numberSb = new StringBuilder();
		while (index < response.length() && Character.isDigit(response.charAt(index))) {
			numberSb.append(response.charAt(index++));
		}
		try {
			return Integer.parseInt(numberSb.toString());
		} catch (NumberFormatException e) {
			return -1;
		}
	}
	
	
	private String getRequest(final String prefix, final int threadId, final int requestId) {
		return String.format("%s%s_%s", prefix, threadId, requestId);
	}
}