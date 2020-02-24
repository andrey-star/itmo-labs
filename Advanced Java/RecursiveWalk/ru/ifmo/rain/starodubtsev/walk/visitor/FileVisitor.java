package ru.ifmo.rain.starodubtsev.walk.visitor;

import ru.ifmo.rain.starodubtsev.walk.hasher.FnvFileHasher;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

import static ru.ifmo.rain.starodubtsev.walk.logger.Logger.error;

public class FileVisitor extends SimpleFileVisitor<Path> {
	
	private final BufferedWriter out;
	
	public FileVisitor(BufferedWriter out) {
		this.out = out;
	}
	
	@Override
	public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
		return writeHash(file, FnvFileHasher.hash(file));
	}
	
	@Override
	public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
		error(exc, "Failed to access file/directory '" + file + "'");
		return writeHash(file, 0);
	}
	
	private FileVisitResult writeHash(Path file, int hash) throws IOException {
		FnvFileHasher.writeHash(out, hash, file.toString());
		return FileVisitResult.CONTINUE;
	}
	
}
