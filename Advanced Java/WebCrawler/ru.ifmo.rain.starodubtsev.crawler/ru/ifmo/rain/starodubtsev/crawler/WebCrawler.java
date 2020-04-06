package ru.ifmo.rain.starodubtsev.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.util.*;
import java.util.concurrent.*;
import java.io.IOException;
import java.net.MalformedURLException;

public class WebCrawler implements Crawler {
	
	private final Downloader downloader;
	private final ExecutorService downloaders;
	private final ExecutorService extractors;
	private final int perHost;
	private final Map<String, HostDownloader> hostDownloaders;
	
	public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
		this.downloader = downloader;
		this.downloaders = Executors.newFixedThreadPool(downloaders);
		this.extractors = Executors.newFixedThreadPool(extractors);
		this.perHost = perHost;
		this.hostDownloaders = new ConcurrentHashMap<>();
	}
	
	public static void main(String[] args) {
		Objects.requireNonNull(args);
		if (args.length == 0) {
			System.out.println("Usage: WebCrawler url [depth [downloaders [extractors [perHost]]]]");
			return;
		}
		try {
			String url = Objects.requireNonNull(args[0]);
			int depth = getArgumentOrDefault(args, 1);
			int downloaders = getArgumentOrDefault(args, 2);
			int extractors = getArgumentOrDefault(args, 3);
			int perHost = getArgumentOrDefault(args, 4);
			try (Crawler wc = new WebCrawler(new CachingDownloader(), downloaders, extractors, perHost)) {
				wc.download(url, depth);
			}
		} catch (NumberFormatException e) {
			System.out.println("Invalid argument(s)");
		} catch (IOException e) {
			System.out.println("An error occurred while initializing downloader");
		}
	}
	
	private static int getArgumentOrDefault(String[] args, int i) {
		if (args.length <= i) {
			return 1;
		}
		Objects.requireNonNull(args[i]);
		return Integer.parseInt(args[i]);
	}
	
	@Override
	public Result download(String url, int depth) {
		return new WebDownloader(url, depth).get();
	}
	
	@Override
	public void close() {
		downloaders.shutdown();
		extractors.shutdown();
		try {
			int timeout = 0;
			downloaders.awaitTermination(timeout, TimeUnit.MILLISECONDS);
			extractors.awaitTermination(timeout, TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	private class WebDownloader {
		
		private final Set<String> downloaded = ConcurrentHashMap.newKeySet();
		private final Map<String, IOException> failed = new ConcurrentHashMap<>();
		private final Set<String> extracted = ConcurrentHashMap.newKeySet();
		
		private final Queue<String> downloadQueue = new ConcurrentLinkedQueue<>();
		private final CountDownLatch complete = new CountDownLatch(1);
		
		public WebDownloader(String url, int depth) {
			downloadQueue.add(url);
			start(depth);
		}
		
		private void start(int depth) {
			for (int i = 0; i < depth; i++) {
				List<String> level = List.copyOf(downloadQueue);
				downloadQueue.clear();
				int toExtract = depth - i - 1;
				Phaser phaser = new Phaser(1);
				level.stream()
				     .filter(extracted::add)
				     .forEach(s -> queueDownload(s, toExtract, phaser));
				phaser.arriveAndAwaitAdvance();
			}
			complete.countDown();
		}
		
		private void queueDownload(String url, int toExtract, Phaser phaser) {
			String host;
			try {
				host = URLUtils.getHost(url);
			} catch (MalformedURLException e) {
				failed.put(url, e);
				return;
			}
			HostDownloader hd = hostDownloaders.computeIfAbsent(host, s -> new HostDownloader());
			phaser.register();
			hd.addTask(() -> {
				try {
					Document document = downloader.download(url);
					downloaded.add(url);
					if (toExtract > 0) {
						queueExtraction(document, phaser);
					}
				} catch (IOException e) {
					failed.put(url, e);
				} finally {
					phaser.arrive();
				}
			});
		}
		
		private void queueExtraction(Document document, Phaser phaser) {
			phaser.register();
			extractors.submit(() -> {
				try {
					downloadQueue.addAll(document.extractLinks());
				} catch (IOException ignored) {
				} finally {
					phaser.arrive();
				}
			});
			
		}
		
		public Result get() {
			try {
				complete.await();
			} catch (InterruptedException e) {
				// TODO
				e.printStackTrace();
			}
			return new Result(List.copyOf(downloaded), failed);
		}
		
	}
	
	private class HostDownloader {
		
		private final Queue<Runnable> downloadQueue;
		private int running;
		
		public HostDownloader() {
			running = 0;
			downloadQueue = new ArrayDeque<>();
		}
		
		public synchronized void addTask(Runnable task) {
			downloadQueue.add(task);
			downloadNext();
		}
		
		private synchronized void downloadNext() {
			if (running < perHost) {
				submit();
			}
		}
		
		private synchronized void submit() {
			Runnable task = downloadQueue.poll();
			if (task != null) {
				running++;
				downloaders.submit(() -> {
					try {
						task.run();
					} finally {
						finished();
					}
				});
			}
		}
		
		private synchronized void finished() {
			running--;
			downloadNext();
		}
	}
}
