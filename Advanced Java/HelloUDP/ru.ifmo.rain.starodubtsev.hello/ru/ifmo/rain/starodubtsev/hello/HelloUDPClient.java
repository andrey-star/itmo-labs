package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * An implementation of the {@code HelloClient} interface.
 * Capable of sending requests to a server via UDP.
 *
 * @see HelloClient
 */
public class HelloUDPClient extends AbstractHelloClient implements HelloClient {
	
	/**
	 * Main method. A command line utility for {@code HelloUDPClient}.
	 * Usage: {@code HelloUDPClient <host> <port> <prefix> <threads> <requests>}.
	 * Creates and runs the {@code client} with the specified parameters.
	 *
	 * @param args program arguments
	 * @see #run(String, int, String, int, int)
	 */
	public static void main(final String[] args) {
		launch(args, HelloUDPClient::new);
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
		waitFor(requestPool);
	}
	
	private void waitFor(final ExecutorService executorService) {
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
	
}
