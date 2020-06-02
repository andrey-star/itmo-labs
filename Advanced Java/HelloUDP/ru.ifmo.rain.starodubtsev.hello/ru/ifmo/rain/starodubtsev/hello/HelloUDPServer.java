package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * An implementation of the {@code HelloServer} interface.
 * Capable of receiving requests and sending responses via UDP.
 *
 * @see HelloServer
 */
public class HelloUDPServer extends AbstractHelloServer implements HelloServer {
	
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
		launch(args, HelloUDPServer::new);
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
			final byte[] receive = new byte[socket.getReceiveBufferSize()];
			final DatagramPacket packet = new DatagramPacket(receive, receive.length);
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
	
	@Override
	public void close() {
		socket.close();
		serverPool.shutdown();
		Utils.waitFor(serverPool, AbstractHelloServer::error);
	}
	
}
