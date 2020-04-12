package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static ru.ifmo.rain.starodubtsev.hello.Logger.error;
import static ru.ifmo.rain.starodubtsev.hello.Logger.log;

public class HelloUDPClient implements HelloClient {
	
	public static void main(String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 5) {
			System.out.println("Usage: HelloUDPClient <host> <port> <prefix> <threads> <requests>");
			return;
		}
		try {
			String host = getArg(0, args);
			int port = getIntArg(1, args);
			String prefix = getArg(2, args);
			int threads = getIntArg(3, args);
			int requests = getIntArg(4, args);
			new HelloUDPClient().run(host, port, prefix, threads, requests);
		} catch (NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	private static String getArg(int i, String[] args) {
		return Objects.requireNonNull(args[i]);
	}
	
	private static int getIntArg(int i, String[] args) {
		return Integer.parseInt(getArg(i, args));
	}
	
	@Override
	public void run(String host, int port, String prefix, int threads, int requests) {
		final SocketAddress socketAddress = new InetSocketAddress(host, port);
		final ExecutorService requestPool = Executors.newFixedThreadPool(threads);
		for (int threadId = 0; threadId < threads; threadId++) {
			final String requestPrefix = getRequestPrefix(prefix, threadId);
			requestPool.submit(() -> processThread(requestPrefix, requests, socketAddress));
		}
		await(requestPool);
	}
	
	private void await(ExecutorService executorService) {
		executorService.shutdown();
		try {
			executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			error(e, "Executing thread was interrupted");
		}
	}
	
	private void processThread(final String prefix, final int requests, final SocketAddress address) {
		try (final DatagramSocket socket = new DatagramSocket()) {
			socket.setSoTimeout(100);
			for (int requestId = 0; requestId < requests; requestId++) {
				final String request = prefix + requestId;
				while (!socket.isClosed() && !Thread.interrupted()) {
					try {
						final String response = PacketUtils.request(request, socket, address);
						if (isResponseValid(request, response)) {
							log(response);
							break;
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
	
	private String getRequestPrefix(String prefix, int threadId) {
		return String.format("%s%s_", prefix, threadId);
	}
	
	private boolean isResponseValid(String request, String response) {
		return response.contains(request);
	}
}
