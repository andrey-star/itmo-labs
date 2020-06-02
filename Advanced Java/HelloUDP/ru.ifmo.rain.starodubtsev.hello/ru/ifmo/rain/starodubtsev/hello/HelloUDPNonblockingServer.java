package ru.ifmo.rain.starodubtsev.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Stream;


public class HelloUDPNonblockingServer extends AbstractHelloServer {
	
	private static DatagramChannel channel;
	private static Selector selector;
	private static ExecutorService pool;
	private static int BUFFER_SIZE;
	
	public static void main(final String[] args) {
		launch(args, HelloUDPNonblockingServer::new);
	}
	
	public void run() {
		while (!Thread.interrupted() && !channel.socket().isClosed()) {
			try {
				selector.select();
				Iterator<SelectionKey> iter = selector.selectedKeys().iterator();
				while (iter.hasNext()) {
					SelectionKey key = iter.next();
					try {
						Attachment attachment = (Attachment) key.attachment();
						if (key.isReadable()) {
							read(key, attachment);
						}
						if (key.isWritable()) {
							write(key, attachment);
						}
					} finally {
						iter.remove();
					}
					
				}
			} catch (IOException e) {
				error(e, "Error occurred during communication with the client");
				close();
			} catch (ClosedSelectorException e) {
				info("The selector was closed");
			}
		}
	}
	
	private void read(SelectionKey key, Attachment attachment) {
		try {
			var requestBuffer = attachment.requestBuffers.remove();
			if (attachment.requestBuffers.isEmpty()) {
				key.interestOpsAnd(~SelectionKey.OP_READ);
			}
			requestBuffer.address = channel.receive(requestBuffer.receive.clear());
			pool.submit(() -> {
				String request = CHARSET.decode(requestBuffer.receive.flip()).toString();
				String response = response(request);
				requestBuffer.send = ByteBuffer.wrap(response.getBytes(CHARSET));
				synchronized (attachment.responseBuffers) {
					attachment.responseBuffers.add(requestBuffer);
					selector.wakeup();
					key.interestOpsOr(SelectionKey.OP_WRITE);
				}
			});
		} catch (IOException e) {
			error(e, "Error occurred when receiving data from a client");
			close();
		}
	}
	
	private void write(SelectionKey key, Attachment attachment) {
		try {
			Attachment.BufferContext sendBuffer;
			synchronized (attachment.responseBuffers) {
				sendBuffer = attachment.responseBuffers.remove();
				if (attachment.responseBuffers.isEmpty()) {
					key.interestOpsAnd(~SelectionKey.OP_WRITE);
				}
			}
			channel.send(sendBuffer.send, sendBuffer.address);
			attachment.requestBuffers.add(sendBuffer);
			key.interestOpsOr(SelectionKey.OP_READ);
		} catch (IOException e) {
			error(e, "Error occurred when sending data to a client");
			close();
		}
	}
	
	@Override
	public void start(int port, int threads) {
		try {
			setup(port, threads);
			pool = Executors.newFixedThreadPool(threads);
			new Thread(this::run).start();
		} catch (IOException e) {
			error(e, "Error occurred during server setup");
		}
	}
	
	private void setup(int port, int threads) throws IOException {
		selector = Selector.open();
		channel = DatagramChannel.open();
		channel.bind(new InetSocketAddress(port));
		channel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
		channel.configureBlocking(false);
		BUFFER_SIZE = channel.socket().getReceiveBufferSize();
		channel.register(selector, SelectionKey.OP_READ, new Attachment(threads));
	}
	
	@Override
	public void close() {
		try {
			channel.close();
			selector.close();
			pool.shutdown();
			Utils.waitFor(pool, AbstractHelloServer::error);
		} catch (IOException e) {
			error(e, "Error occurred when attempting to close resources");
		}
	}
	
	private static class Attachment {
		private final Queue<BufferContext> requestBuffers;
		private final Queue<BufferContext> responseBuffers;
		
		private Attachment(int threads) {
			this.requestBuffers = new ArrayDeque<>(threads);
			Stream.generate(BufferContext::new).limit(threads).forEach(requestBuffers::add);
			this.responseBuffers = new ArrayDeque<>();
			
		}
		
		private static class BufferContext {
			ByteBuffer receive;
			ByteBuffer send;
			SocketAddress address;
			
			public BufferContext() {
				receive = ByteBuffer.allocate(BUFFER_SIZE);
			}
		}
	}
	
}