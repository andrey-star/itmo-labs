package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static ru.ifmo.rain.starodubtsev.hello.Logger.error;

public class HelloUDPServer implements HelloServer {
	
	private ExecutorService serverPool;
	private DatagramSocket socket;
	
	public static void main(final String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 2) {
			System.out.println("Usage: HelloUDPServer <port> <threads>");
			return;
		}
		try {
			final int port = getIntArg(0, args);
			final int threads = getIntArg(1, args);
			final HelloServer server = new HelloUDPServer();
			server.start(port, threads);
		} catch (final NumberFormatException e) {
			error(e, "Invalid argument(s)");
		}
	}
	
	private static String getArg(final int i, final String[] args) {
		return Objects.requireNonNull(args[i]);
	}
	
	private static int getIntArg(final int i, final String[] args) {
		return Integer.parseInt(getArg(i, args));
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
		while (!socket.isClosed() && !Thread.interrupted()) {
			try {
				final DatagramPacket packet = PacketUtils.createPacket(socket);
				socket.receive(packet);
				final String request = PacketUtils.getString(packet);
				PacketUtils.setString(packet, response(request));
				socket.send(packet);
			} catch (final IOException e) {
				if (!socket.isClosed()) {
					error(e, "Error occurred during communication with the client");
				}
			}
		}
	}
	
	private String response(final String request) {
		return "Hello, " + request;
	}
	
	@Override
	public void close() {
		socket.close();
		serverPool.shutdown();
		try {
			serverPool.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
		} catch (final InterruptedException e) {
			error(e, "Executing thread interrupted when stopping server");
		}
	}
}
