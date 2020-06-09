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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static ru.ifmo.rain.starodubtsev.hello.Utils.CHARSET;

public class HelloUDPNonblockingServer extends AbstractHelloServer {
	
	private ExecutorService pool;
	private int bufSize;
	private DatagramChannel channel;
	private Selector selector;
	
	public static void main(final String[] args) {
		start(args, HelloUDPNonblockingServer::new);
	}
	
	public void run() {
		while (!Thread.interrupted() && !channel.socket().isClosed()) {
			try {
				selector.select();
				Iterator<SelectionKey> iter = selector.selectedKeys().iterator();
				while (iter.hasNext()) {
					SelectionKey key = iter.next();
					try {
						if (key.isReadable()) {
							read(key);
						}
						if (key.isWritable()) {
							write(key);
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
	
	private void read(SelectionKey key) {
		try {
			Attachment attachment = (Attachment) key.attachment();
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
					if ((key.interestOps() & SelectionKey.OP_WRITE) == 0) {
						key.interestOpsOr(SelectionKey.OP_WRITE);
						selector.wakeup();
					}
				}
			});
		} catch (IOException e) {
			error(e, "Error occurred when receiving data from a client");
			close();
		}
	}
	
	private void write(SelectionKey key) {
		try {
			Attachment attachment = (Attachment) key.attachment();
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
		setup(port, threads);
		new Thread(this::run).start();
	}
	
	private void setup(int port, int threads) {
		try {
			selector = Selector.open();
			channel = DatagramChannel.open();
			channel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
			channel.configureBlocking(false);
			bufSize = channel.socket().getReceiveBufferSize();
			channel.register(selector, SelectionKey.OP_READ, new Attachment(threads));
			channel.bind(new InetSocketAddress(port));
			pool = Executors.newFixedThreadPool(threads);
		} catch (IOException e) {
			error(e, "Error occurred during server setup");
		}
	}
	
	@Override
	public void close() {
		try {
			if (channel != null) {
				channel.close();
			}
			if (selector != null) {
				selector.close();
			}
			Utils.waitFor(pool, AbstractHelloServer::error);
		} catch (IOException e) {
			error(e, "Error occurred when attempting to close resources");
		}
	}
	
	private class Attachment {
		private final Queue<BufferContext> requestBuffers;
		private final Queue<BufferContext> responseBuffers;
		
		private Attachment(int threads) {
			this.requestBuffers = Stream.generate(BufferContext::new)
					.limit(threads)
					.collect(Collectors.toCollection(ArrayDeque::new));
			this.responseBuffers = new ArrayDeque<>();
			
		}
		
		private class BufferContext {
			ByteBuffer receive;
			ByteBuffer send;
			SocketAddress address;
			
			public BufferContext() {
				receive = ByteBuffer.allocate(bufSize);
			}
		}
	}
	
}