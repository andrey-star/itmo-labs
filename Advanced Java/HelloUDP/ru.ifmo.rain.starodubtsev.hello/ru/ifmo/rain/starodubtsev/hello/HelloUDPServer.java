package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static ru.ifmo.rain.starodubtsev.hello.MainUtils.getIntArg;

/**
 * An implementation of the {@code HelloServer} interface.
 * Capable of receiving requests and sending responses via UDP.
 *
 * @see HelloServer
 */
public class HelloUDPServer implements HelloServer {
	
	public static final long AWAIT_TERMINATION_MILLISECONDS = Long.MAX_VALUE;
	private ExecutorService serverPool;
	private DatagramSocket socket;
	
	/**
	 * Main method. A command line utility for {@code HelloUDPServer}.
	 * Usage: {@code HelloUDPServer <port> <threads>}.
	 * Creates and starts the {@code server} with the specified parameters.
	 *
	 * @param args program arguments
	 * @see #start(int, int)
	 */
	public static void main(final String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 2) {
			info("Usage: HelloUDPServer <port> <threads>");
			return;
		}
		try {
			final int port = getIntArg(0, args);
			final int threads = getIntArg(1, args);
			try (final HelloUDPServer server = new HelloUDPServer()) {
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
	
	private static void error(final Exception e, final String message) {
		Logger.error(e, "[Server]", message);
	}
	
	private static void info(final String message) {
		Logger.info("[Server]", message);
	}
	
	@Override
	public void start(final int port, final int threads) {
		serverPool = Executors.newFixedThreadPool(threads);
		try {
			socket = new DatagramSocket(port);
			for (int i = 0; i < threads; i++) {
				serverPool.submit(this::processRequest);
			}
		} catch (final SocketException e) {
			error(e, "Error when creating socket");
		}
	}
	
	private void processRequest() {
		try {
			final DatagramPacket packet = new DatagramPacket(new byte[0], 0);
			final byte[] receive = new byte[socket.getReceiveBufferSize()];
			while (!socket.isClosed() && !Thread.interrupted()) {
				try {
					final String request = DatagramUtils.setDataAndReceive(receive, packet, socket);
					DatagramUtils.send(response(request), packet, socket);
				} catch (final SocketTimeoutException ignored) {
					info("Server did not receive any requests in given time");
				} catch (final IOException e) {
					error(e, "Error occurred during communication with the client");
				}
			}
		} catch (final SocketException e) {
			error(e, "Error when creating UDP packet");
		}
	}
	
	private String response(final String request) {
		return "Hello, " + request;
	}
	
	@Override
	public void close() {
		socket.close();
		serverPool.shutdown();
		while (true) {
			try {
				serverPool.awaitTermination(AWAIT_TERMINATION_MILLISECONDS, TimeUnit.MILLISECONDS);
				break;
			} catch (final InterruptedException e) {
				error(e, "Executing thread interrupted during server shutdown");
			}
		}
	}
}
