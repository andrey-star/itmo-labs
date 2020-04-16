package ru.ifmo.rain.starodubtsev.walk.hasher;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

import static ru.ifmo.rain.starodubtsev.walk.logger.Logger.error;

public class FnvFileHasher {
	
	private static final int FNV_PRIME = 0x01000193;
	private static final int FNV_X0 = 0x811c9dc5;
	
	private static final int BUF_SIZE = 0xffff;
	private static final byte[] buffer = new byte[BUF_SIZE];
	
	public static int hash(final Path path) {
		try (final InputStream inputStream = new BufferedInputStream(Files.newInputStream(path))) {
			int hash = FNV_X0;
//			try {
				int bytesRead = 0;
				while (bytesRead != -1) {
					for (int i = 0; i < bytesRead; i++) {
						hash = (hash * FNV_PRIME) ^ (buffer[i] & 0xff);
					}
					bytesRead = inputStream.read(buffer);
				}
//			} catch (IOException e) {
//				hasherError(e, "Error when reading input from file '" + path + "'.");
//				hash = 0;
//			}
			return hash;
		} catch (final NoSuchFileException e) {
			hasherError(e, "No such file found '" + path + "'.");
		} catch (final SecurityException e) {
			hasherError(e, "Unable to access file '" + path + "'.");
		} catch (final IOException e) {
			hasherError(e, "Error when reading input from file '" + path + "'.");
		}
		return 0;
	}
	
	private static void hasherError(final Exception e, final String message) {
		error(e, message + " hash set to default (0)");
	}
	
	public static void writeHash(final BufferedWriter out, final int hash, final String file) throws IOException {
		out.write(String.format("%08x %s\n", hash, file));
	}
	
}