package ru.ifmo.rain.starodubtsev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class ImplementorUtils {
	
	public static void createDirectories(Path file) throws ImplerException {
		Path parent = file.getParent();
		if (parent != null) {
			try {
				Files.createDirectories(parent);
			} catch (IOException e) {
				throw new ImplerException("Cannot create", e);
			}
		}
	}
	
}
