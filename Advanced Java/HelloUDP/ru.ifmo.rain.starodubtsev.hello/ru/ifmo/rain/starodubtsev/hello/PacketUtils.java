package ru.ifmo.rain.starodubtsev.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class PacketUtils {
	
	public static final Charset CHARSET = StandardCharsets.UTF_8;
	
	public static String getString(final DatagramPacket packet) {
		return new String(packet.getData(), packet.getOffset(), packet.getLength(), CHARSET);
	}
	
	public static void setString(final DatagramPacket packet, final String string) {
		final byte[] bytes = string.getBytes(CHARSET);
		packet.setData(bytes);
		packet.setLength(packet.getData().length);
	}
	
	public static String request(final String request, final DatagramSocket datagramSocket, final SocketAddress socketAddress) throws IOException {
		send(request, datagramSocket, socketAddress);
		return receive(datagramSocket);
	}
	
	public static void send(final String request, final DatagramSocket socket, final SocketAddress address) throws IOException {
		final DatagramPacket outPacket = new DatagramPacket(new byte[0], 0);
		setString(outPacket, request);
		outPacket.setSocketAddress(address);
		socket.send(outPacket);
	}
	
	public static String receive(final DatagramSocket socket) throws IOException {
		DatagramPacket packet = createPacket(socket);
		socket.receive(packet);
		return getString(packet);
	}
	
	public static DatagramPacket createPacket(final DatagramSocket socket) throws SocketException {
		return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
	}
}
