package ru.ifmo.rain.starodubtsev.walk;

import ru.ifmo.rain.starodubtsev.walk.exception.RecursiveWalkException;
import ru.ifmo.rain.starodubtsev.walk.hasher.FnvFileHasher;
import ru.ifmo.rain.starodubtsev.walk.visitor.FileVisitor;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

import static ru.ifmo.rain.starodubtsev.walk.logger.Logger.error;

public class RecursiveWalk {
	
	public static void main(String[] args) {
		
		if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
			System.out.println("Usage: java RecursiveWalk <input file> <output file>");
			return;
		}
		
		try {
			Path inputFile = Paths.get(args[0]);
			try {
				Path outputFile = Paths.get(args[1]);
				run(inputFile, outputFile);
			} catch (InvalidPathException e) {
				error(e, "Invalid output file '" + args[1] + "'");
			} catch (RecursiveWalkException e) {
				error(e, e.getMessage());
			}
		} catch (InvalidPathException e) {
			error(e, "Invalid input file '" + args[0] + "'");
		}
	}
	
	private static void run(Path inputFile, Path outputFile) throws RecursiveWalkException {
		try (BufferedReader in = Files.newBufferedReader(inputFile)) {
			try (BufferedWriter out = Files.newBufferedWriter(outputFile)) {
				try { // readline
					FileVisitor fileVisitor = new FileVisitor(out);
					
					String pathStr;
					while ((pathStr = in.readLine()) != null) {
						try {
							try { // path
								Path rootPath = Paths.get(pathStr);
								try {
									Files.walkFileTree(rootPath, fileVisitor);
								} catch (SecurityException e) {
									error(e, "Unable to access file/directory '" + pathStr + "'");
								}
							} catch (InvalidPathException e) { // path
								error(e, "Invalid input path '" + pathStr + "'");
								FnvFileHasher.writeHash(out, 0, pathStr);
							}
						} catch (IOException e) { // writeHash from walkFileTree and from InvalidPath
							throw rwe("Error when writing output to file '" + outputFile + "'", e);
						}
					}
				} catch (IOException e) { // readline
					throw rwe("Error when reading input from file '" + inputFile + "'", e);
				}
			} catch (SecurityException e) { // out
				throw rwe("Unable to access output file '" + outputFile.toString() + "'", e);
			} catch (IOException e) {
				throw rwe("Error when opening/creating output file '" + outputFile + "'", e);
			}
		} catch (SecurityException e) { // in
			throw rwe("Unable to access input file " + inputFile.toString(), e);
		} catch (IOException e) {
			throw rwe("Error when opening input file '" + inputFile + "'", e);
		}
	}
	
	private static RecursiveWalkException rwe(String message, Throwable e) {
		return new RecursiveWalkException(message, e);
	}
	
}
