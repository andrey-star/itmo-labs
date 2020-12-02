package com.andreystar;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

class ParserTest {
	
	@Test
	public void test1() throws IOException {
		test(1);
	}
	
	@Test
	public void test2() throws IOException {
		test(2);
	}
	
	@Test
	public void test3() throws IOException {
		test(3);
	}
	
	private void test(int i) throws IOException {
		String input = Files.readString(Path.of("tests/test" + i + ".in"));
		String expected = Files.readString(Path.of("tests/test" + i + ".out")).replace("\r\n", "\n");
		assertEquals(expected, ParserUtils.parseFromString(input).stripLeading());
	}
	
}