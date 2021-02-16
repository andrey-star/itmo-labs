package com.andreystar.util;

import java.util.stream.Collectors;

public class StringUtils {
	
	public static String formatCode(String s) {
		int[] curTabLevel = {0};
		return s.lines()
				.map(String::strip)
				.map(line -> {
					if (line.startsWith("}") || line.startsWith(")")) {
						curTabLevel[0]--;
						if (curTabLevel[0] < 0) {
							curTabLevel[0] = 0;
						}
					}
					String res = "\t".repeat(curTabLevel[0]) + line;
					if (line.endsWith("{") || line.endsWith("(")) {
						curTabLevel[0]++;
					}
					return res;
				}).collect(Collectors.joining("\n"));
	}
	
}
