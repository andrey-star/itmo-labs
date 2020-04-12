package ru.ifmo.rain.starodubtsev.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.util.*;
import java.util.concurrent.*;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class WebCrawler implements Crawler {
	
	private static final boolean PRINT_STACK_TRACE = false;
	
	private final Downloader downloader;
	private final ExecutorService downloaders;
	private final ExecutorService extractors;
	private final int perHost;
	private final Map<String, Semaphore> hostLimiters;
	
	/**
	 * Constructs an instance of {@code WebCrawler} with the specified {@code Downloader}
	 * and parallelism parameters.
	 *
	 * @param downloader  the page downloader
	 * @param downloaders the amount of parallel downloaders
	 * @param extractors  the amount of parallel extractors
	 * @param perHost     the amount of parallel connections per host
	 */
	public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
		this.downloader = downloader;
		this.downloaders = Executors.newFixedThreadPool(downloaders);
		this.extractors = Executors.newFixedThreadPool(extractors);
		this.perHost = perHost;
		this.hostLimiters = new ConcurrentHashMap<>();
	}
	
	/**
	 * Main method. Usage: WebCrawler url [depth [downloaders [extractors [perHost]]]].
	 * Downloads the provided page with the specified parameters (1 by default).
	 *
	 * @param args program arguments
	 * @see #download(String, int)
	 */
	public static void main(final String[] args) {
		Objects.requireNonNull(args);
		if (args.length == 0) {
			System.out.println("Usage: WebCrawler url [depth [downloaders [extractors [perHost]]]]");
			return;
		}
		try {
			final String url = Objects.requireNonNull(args[0]);
			final int depth = getArgumentOrDefault(args, 1);
			final int downloaders = getArgumentOrDefault(args, 2);
			final int extractors = getArgumentOrDefault(args, 3);
			final int perHost = getArgumentOrDefault(args, 4);
			try (final Crawler wc = new WebCrawler(new CachingDownloader(), downloaders, extractors, perHost)) {
				wc.download(url, depth);
			}
		} catch (final NumberFormatException e) {
			System.out.println("Invalid argument(s)");
		} catch (final IOException e) {
			System.out.println("An error occurred while initializing downloader");
		}
	}
	
	private static int getArgumentOrDefault(final String[] args, final int i) {
		if (args.length <= i) {
			return 1;
		}
		Objects.requireNonNull(args[i]);
		return Integer.parseInt(args[i]);
	}
	
	@Override
	public Result download(final String url, final int depth) {
		return new WebDownloader(url).download(depth);
	}
	
	@Override
	public void close() {
		downloaders.shutdown();
		extractors.shutdown();
		while (true) {
			try {
				final long timeout = Long.MAX_VALUE;
				downloaders.awaitTermination(timeout, TimeUnit.MILLISECONDS);
				extractors.awaitTermination(timeout, TimeUnit.MILLISECONDS);
				break;
			} catch (final InterruptedException e) {
				if (PRINT_STACK_TRACE) {
					e.printStackTrace();
				}
			}
		}
	}
	
	private static class SwappingQueue<T> {
		
		private final Queue<T> first;
		private final Queue<T> second;
		private Queue<T> cur;
		
		public SwappingQueue(Supplier<Queue<T>> queueSupplier) {
			this.first = queueSupplier.get();
			this.second = queueSupplier.get();
			cur = first;
		}
		
		public void addAll(Collection<T> c) {
			cur.addAll(c);
		}
		
		public void add(T t) {
			cur.add(t);
		}
		
		public Queue<T> removeAll() {
			if (cur == first) {
				second.clear();
				cur = second;
				return first;
			}
			first.clear();
			cur = first;
			return second;
		}
	}
	
	private class WebDownloader {
		
		private final SwappingQueue<String> downloadQueue = new SwappingQueue<>(ConcurrentLinkedQueue::new);
		private final Map<String, IOException> failed = new ConcurrentHashMap<>();
		private final Set<String> extracted = ConcurrentHashMap.newKeySet();
		private final Set<String> downloaded = ConcurrentHashMap.newKeySet();
		private final Phaser phaser = new Phaser(1);
		
		public WebDownloader(final String url) {
			downloadQueue.add(url);
		}
		
		public Result download(final int depth) {
			downloadRecursively(depth - 1);
			return new Result(List.copyOf(downloaded), Map.copyOf(failed));
		}
		
		public void downloadRecursively(final int left) {
			final Queue<String> level = downloadQueue.removeAll();
			level.stream()
			     .filter(extracted::add)
			     .forEach(url -> queueDownload(url, left > 0 ? this::queueExtraction : this::emptyConsumer));
			phaser.arriveAndAwaitAdvance();
			if (left > 0) {
				downloadRecursively(left - 1);
			}
		}
		
		private void queueDownload(final String url, Consumer<Document> extractor) {
			final String host;
			try {
				host = URLUtils.getHost(url);
			} catch (final MalformedURLException e) {
				failed.put(url, e);
				return;
			}
			final Semaphore hostLimiter = hostLimiters.computeIfAbsent(host, s -> new Semaphore(perHost));
			phaser.register();
			downloaders.submit(() -> {
				try {
					hostLimiter.acquireUninterruptibly();
					final Document document = downloader.download(url);
					downloaded.add(url);
					extractor.accept(document);
				} catch (final IOException e) {
					failed.put(url, e);
				} finally {
					hostLimiter.release();
					phaser.arriveAndDeregister();
				}
			});
		}
		
		private void queueExtraction(final Document document) {
			phaser.register();
			extractors.submit(() -> {
				try {
					downloadQueue.addAll(document.extractLinks());
				} catch (final IOException ignored) {
				} finally {
					phaser.arriveAndDeregister();
				}
			});
		}
		
		private <T> void emptyConsumer(T t) {
		}
		
	}
	
}
