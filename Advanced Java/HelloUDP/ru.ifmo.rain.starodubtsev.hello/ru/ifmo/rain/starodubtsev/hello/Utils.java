package ru.ifmo.rain.starodubtsev.hello;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;

public class Utils {
	
	public static final Charset CHARSET = StandardCharsets.UTF_8;
	
	private static final long AWAIT_TERMINATION_MS = Long.MAX_VALUE;
	
	public static String getArg(final int i, final String[] args) {
		return Objects.requireNonNull(args[i]);
	}
	
	public static int getIntArg(final int i, final String[] args) {
		return Integer.parseInt(getArg(i, args));
	}
	
	public static void waitFor(ExecutorService service, BiConsumer<Exception, String> exceptionHandler) {
		if (service == null) {
			return;
		}
		service.shutdown();
		while (true) {
			try {
				service.awaitTermination(AWAIT_TERMINATION_MS, TimeUnit.MILLISECONDS);
				break;
			} catch (final InterruptedException e) {
				exceptionHandler.accept(e, "Executing thread interrupted during server shutdown");
			}
		}
	}
	
}
