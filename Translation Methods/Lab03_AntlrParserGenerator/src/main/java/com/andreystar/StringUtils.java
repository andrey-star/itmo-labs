package com.andreystar;

import java.util.stream.Collectors;

public class StringUtils {
	public static String tabbed(String body, int amount) {
		var tabs = "\t".repeat(amount);
		return body.lines()
				.map(l -> tabs + l)
				.collect(Collectors.joining("\n"));
	}
}
