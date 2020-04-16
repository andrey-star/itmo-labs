package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
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
	
	private static final int SO_TIMEOUT = 100;
	private ExecutorService serverPool;
	private DatagramSocket socket;
	
	/**
	 * Main method. Usage: {@code Usage: HelloUDPServer <port> <threads>}.
	 * Creates and starts the {@code server} with the specified parameters.
	 *
	 * @param args program arguments
	 * @see #start(int, int)
	 */
	public static void main(final String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 2) {
			System.out.println("Usage: HelloUDPServer <port> <threads>");
			return;
		}
		try {
			final int port = getIntArg(0, args);
			final int threads = getIntArg(1, args);
			try (HelloUDPServer server = new HelloUDPServer()) {
				server.start(port, threads);
			}
		} catch (final NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	private static void error(Exception e, String message) {
		Logger.error(e, "[Server]", message);
	}
	
	private static void log(String message) {
		Logger.log("[Server]", message);
	}
	
	@Override
	public void start(final int port, final int threads) {
		serverPool = Executors.newFixedThreadPool(threads);
		try {
			socket = new DatagramSocket(port);
			socket.setSoTimeout(SO_TIMEOUT);
			for (int i = 0; i < threads; i++) {
				serverPool.submit(this::processRequest);
			}
		} catch (final SocketException e) {
			error(e, "Error when creating socket");
		}
	}
	
	private void processRequest() {
		try {
			final DatagramPacket packet = DatagramUtils.createPacket(socket);
			while (!socket.isClosed() && !Thread.interrupted()) {
				try {
					socket.receive(packet);
					final String request = DatagramUtils.getString(packet);
//					log("Server received request: " + request);
					final String response = response(request);
					DatagramUtils.setString(response, packet);
					socket.send(packet);
//					log("Server sent response: " + response);
				} catch (SocketTimeoutException ignored) {
//					log("Server did not receive any requests in given time");
				} catch (final IOException e) {
					error(e, "Error occurred during communication with the client");
				}
			}
		} catch (SocketException e) {
			error(e, "Error when creating UDP packet");
		}
		
	}
	
	private String response(final String request) {
		return "Hello, " + request;
	}
	
	@Override
	public void close() {
		serverPool.shutdownNow();
		while (true) {
			try {
				serverPool.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
				break;
			} catch (final InterruptedException e) {
				error(e, "Executing thread interrupted during server shutdown");
			}
		}
		socket.close();
	}
}