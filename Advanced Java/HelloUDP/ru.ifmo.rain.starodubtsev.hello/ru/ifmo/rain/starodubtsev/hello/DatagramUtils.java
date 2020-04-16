package ru.ifmo.rain.starodubtsev.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Utility class providing helper methods for {@link DatagramSocket} and {@link DatagramPacket} classes.
 */
public class DatagramUtils {
	
	public static final Charset CHARSET = StandardCharsets.UTF_8;
	
	public static String getString(final DatagramPacket packet) {
		return new String(packet.getData(), packet.getOffset(), packet.getLength(), CHARSET);
	}
	
	public static void setString(final String string, final DatagramPacket packet) {
		final byte[] bytes = string.getBytes(CHARSET);
		packet.setData(bytes);
		packet.setLength(bytes.length);
	}
	
	public static String request(final String request, final DatagramPacket packet, final DatagramSocket datagramSocket, final SocketAddress address) throws IOException {
		send(request, packet, datagramSocket, address);
		return receive(packet, datagramSocket);
	}
	
	public static void send(final String request, final DatagramPacket packet, final DatagramSocket socket, final SocketAddress address) throws IOException {
		setString(request, packet);
		packet.setSocketAddress(address);
		socket.send(packet);
	}
	
	public static String receive(final DatagramPacket packet, final DatagramSocket socket) throws IOException {
		resizeToReceive(packet, socket);
		socket.receive(packet);
		return getString(packet);
	}
	
	private static void resizeToReceive(final DatagramPacket packet, final DatagramSocket socket) throws SocketException {
		packet.setData(new byte[socket.getReceiveBufferSize()]);
		packet.setLength(packet.getData().length);
	}
	
	public static DatagramPacket createPacket(final DatagramSocket socket) throws SocketException {
		return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
	}
}