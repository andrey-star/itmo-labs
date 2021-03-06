package ru.ifmo.rain.starodubtsev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static ru.ifmo.rain.starodubtsev.hello.Utils.CHARSET;

public class HelloUDPNonblockingClient extends AbstractHelloClient implements HelloClient {
	
	private int bufSize;
	
	public static void main(final String[] args) {
		run(args, HelloUDPNonblockingClient::new);
	}
	
	@Override
	public void run(String host, int port, String prefix, int threads, int requests) {
		List<DatagramChannel> channels = new ArrayList<>();
		try (Selector selector = Selector.open()) {
			SocketAddress address = new InetSocketAddress(host, port);
			for (int i = 0; i < threads; i++) {
				DatagramChannel channel = DatagramChannel.open();
				channel.configureBlocking(false);
				channel.connect(address);
				bufSize = channel.socket().getReceiveBufferSize();
				channel.register(selector, SelectionKey.OP_WRITE, new Attachment(i));
				channels.add(channel);
			}
			while (!Thread.interrupted() && !selector.keys().isEmpty()) {
				selector.select(SO_TIMEOUT);
				final Set<SelectionKey> selectionKeys = selector.selectedKeys();
				if (selectionKeys.isEmpty()) {
					for (SelectionKey selectionKey : selector.keys()) {
						selectionKey.interestOps(SelectionKey.OP_WRITE);
					}
				}
				Iterator<SelectionKey> iter = selectionKeys.iterator();
				while (iter.hasNext()) {
					SelectionKey key = iter.next();
					try {
						DatagramChannel channel = (DatagramChannel) key.channel();
						Attachment attachment = (Attachment) key.attachment();
						if (key.isWritable()) {
							channel.send(ByteBuffer.wrap(getRequest(prefix, attachment.threadId, attachment.requestId).getBytes(CHARSET)), address);
							key.interestOps(SelectionKey.OP_READ);
						}
						if (key.isReadable()) {
							channel.receive(attachment.buffer.clear());
							String response = CHARSET.decode(attachment.buffer.flip()).toString();
							if (isResponseValid(response, attachment.threadId, attachment.requestId)) {
								attachment.requestId++;
							}
							key.interestOps(SelectionKey.OP_WRITE);
							if (attachment.requestId >= requests) {
								channel.close();
							}
						}
					} finally {
						iter.remove();
					}
				}
			}
		} catch (IOException e) {
			error(e, "Error occurred during communication with the server");
		} finally {
			for (DatagramChannel channel : channels) {
				try {
					channel.close();
				} catch (IOException e) {
					error(e, "Error occurred when trying to close channels");
				}
			}
		}
	}
	
	private class Attachment {
		
		private final int threadId;
		private final ByteBuffer buffer;
		private int requestId = 0;
		
		public Attachment(int threadId) {
			this.threadId = threadId;
			buffer = ByteBuffer.allocate(bufSize);
		}
		
	}
}