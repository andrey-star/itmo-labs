package ru.ifmo.rain.starodubtsev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * Class providing file and directory management utilities for {@link Implementor}.
 *
 * @author Andrey Starodubtsev
 */
public class ImplementorUtils {
	
	/**
	 * A {@code DeletingFileVisitor} object used to delete file trees.
	 */
	private static DeletingFileVisitor deletingFileVisitor = new DeletingFileVisitor();
	
	/**
	 * Prevents this class from being instantiated.
	 */
	private ImplementorUtils() {
	}
	
	/**
	 * Creates all directories required by the provided {@code file}.
	 *
	 * @param file a {@code Path} locating the desired file
	 * @throws ImplerException if an error occurs during directories creation
	 */
	public static void createDirectories(Path file) throws ImplerException {
		Path parent = file.toAbsolutePath().getParent();
		if (parent != null) {
			try {
				Files.createDirectories(parent);
			} catch (IOException e) {
				throw new ImplerException("Cannot create directory: " + parent, e);
			}
		}
	}
	
	/**
	 * Creates a temporary directory at the specified location.
	 *
	 * @param root the location for the temporary directory
	 * @return a {@code Path} object locating the created temporary directory
	 * @throws ImplerException if an error occurs when creating the temporary directory,
	 *                         or an invalid path was provided
	 */
	public static Path createTempDirectory(Path root) throws ImplerException {
		if (root == null) {
			throw new ImplerException("Cannot create directory: ");
		}
		root = root.toAbsolutePath();
		try {
			return Files.createTempDirectory(root, "jar-implementor");
		} catch (IOException e) {
			throw new ImplerException("Could not create temporary directory");
		}
	}
	
	/**
	 * Deletes the specified directory and all its contents.
	 *
	 * @param root the location of the directory to be deleted
	 * @throws ImplerException if an error occurs during directory deletion
	 */
	public static void cleanDirectory(Path root) throws ImplerException {
		if (root == null) {
			return;
		}
		root = root.toAbsolutePath();
		try {
			Files.walkFileTree(root, deletingFileVisitor);
		} catch (IOException e) {
			throw new ImplerException("Could not clean temporary directory at: " + root);
		}
	}
	
	/**
	 * An implementation of the {@code FileVisitor} interface. Capable of deleting
	 * file trees. Used with {@link Files#walkFileTree(Path, FileVisitor)}.
	 *
	 * @see java.nio.file.FileVisitor
	 * @see SimpleFileVisitor
	 * @see Files#walkFileTree(Path, FileVisitor)
	 */
	private static class DeletingFileVisitor extends SimpleFileVisitor<Path> {
		/**
		 * Default constructor. Creates a new instance of {@code DeletingFileVisitor}.
		 */
		private DeletingFileVisitor() {
		}
		
		/**
		 * File visitor method. Deletes the specified {@code file}.
		 *
		 * @throws IOException if the method fails to delete the specified {@code file}
		 */
		@Override
		public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
			Files.delete(file);
			return FileVisitResult.CONTINUE;
		}
		
		/**
		 * Directory visitor method. Deletes the specified {@code dir}.
		 *
		 * @throws IOException if the method fails to delete the specified {@code dir}, or
		 *                     any file under it
		 */
		@Override
		public FileVisitResult postVisitDirectory(Path dir, IOException e) throws IOException {
			if (e == null) {
				Files.delete(dir);
				return FileVisitResult.CONTINUE;
			} else {
				throw e;
			}
		}
	}
}