package ru.ifmo.rain.starodubtsev.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
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
		packet.setData(string.getBytes(CHARSET));
	}
	
	public static void send(final String request, final DatagramPacket packet, final DatagramSocket socket) throws IOException {
		setString(request, packet);
		socket.send(packet);
	}
	
	public static String setDataAndReceive(final byte[] receive, final DatagramPacket packet, final DatagramSocket socket) throws IOException {
		packet.setData(receive);
		socket.receive(packet);
		return getString(packet);
	}
}
