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
	
	public static int hash(Path path) {
		int hash = FNV_X0;
		try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(path))) {
			try {
				byte[] b = new byte[BUF_SIZE];
				int length = inputStream.read(b);
				while (length != -1) {
					for (int i = 0; i < length; i++) {
						hash = (hash * FNV_PRIME) ^ (b[i] & 0xff);
					}
					length = inputStream.read(b);
				}
			} catch (IOException e) {
				hasherError(e, "Error when reading input from file '" + path + "'.");
				hash = 0;
			}
		} catch (NoSuchFileException e) {
			hasherError(e, "No such file found '" + path + "'.");
			hash = 0;
		} catch (SecurityException e) {
			hasherError(e, "Unable to access file '" + path + "'.");
			hash = 0;
		} catch (IOException e) {
			hasherError(e, "Error when opening file '" + path + "'.");
			hash = 0;
		}
		return hash;
	}
	
	private static void hasherError(Exception e, String message) {
		error(e, message + " hash set to default (0)");
	}
	
	public static void writeHash(BufferedWriter out, int hash, String file) throws IOException {
		out.write(String.format("%08x %s\n", hash, file));
	}
	
}
