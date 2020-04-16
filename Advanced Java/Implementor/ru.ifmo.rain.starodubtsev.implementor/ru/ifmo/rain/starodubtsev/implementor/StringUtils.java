package ru.ifmo.rain.starodubtsev.implementor;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;
import java.util.stream.Collectors;

public class StringUtils {
	
	/**
	 * System defined line separator for generated {@code .java} files.
	 */
	private static final String LINE_SEP = System.lineSeparator();
	/**
	 * Tab code indentation for generated {@code .java} files.
	 */
	private static final String TAB = "\t";
	
	/**
	 * Combines {@code declaration} and {@code body} into properly formatted {@code String}.
	 * <p>
	 * Resulting formatting:
	 * <pre>
	 *  declaration {
	 *      body
	 *  }
	 * </pre>
	 *
	 * @param declaration the declaration used when formatting
	 * @param body        the body used when formatting
	 * @return the resulting formatted {@code String}
	 */
	static String declAndBody(String declaration, String body) {
		return String.format("%s {%s%s%s}", declaration, LINE_SEP, tabbed(body, 1), LINE_SEP);
	}
	
	/**
	 * Tabulates every line of {@code body} exactly {@code amount} number of times.
	 *
	 * @param body   the {@code String} to tabulate
	 * @param amount number, specifying the amount of tabs
	 * @return the tabbed {@code String}
	 */
	private static String tabbed(String body, int amount) {
		var tabs = TAB.repeat(amount);
		return body.lines()
		           .map(l -> tabs + l)
		           .collect(Collectors.joining(LINE_SEP));
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by one blank line.
	 *
	 * @param blocks the {@code String}s to separate
	 * @return the separated {@code String}
	 * @see #joinBlocks(Object[], Function)
	 */
	static String joinBlocks(String... blocks) {
		return joinBlocks(blocks, Function.identity());
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by one blank line,
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks   the objects to separate
	 * @param toString mapping function for provided objects
	 * @param <T>      the type of elements in {@code blocks} array
	 * @return the separated {@code String}
	 * @see #joinBlocks(Collection, Function)
	 */
	private static <T> String joinBlocks(T[] blocks, Function<T, String> toString) {
		return joinBlocks(Arrays.asList(blocks), toString);
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by one blank line,
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks   the objects to separate
	 * @param toString mapping function for provided objects
	 * @param <T>      the type of elements in {@code blocks} collection
	 * @return the separated {@code String}
	 */
	static <T> String joinBlocks(Collection<T> blocks, Function<T, String> toString) {
		return join(blocks, toString, LINE_SEP.repeat(2));
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by {@code separator},
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks    the objects to separate
	 * @param toString  mapping function for provided objects
	 * @param separator the separator used between each element
	 * @param <T>       the type of elements in {@code blocks} array
	 * @return the separated {@code String}
	 * @see #join(Collection, Function, String)
	 */
	static <T> String join(T[] blocks, Function<T, String> toString, String separator) {
		return join(Arrays.asList(blocks), toString, separator);
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by {@code separator},
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks    the objects to separate
	 * @param toString  mapping function for provided objects
	 * @param separator the separator used between each element
	 * @param <T>       the type of elements in {@code blocks} collection
	 * @return the separated {@code String}
	 * @see Collectors#joining(CharSequence)
	 */
	private static <T> String join(Collection<T> blocks, Function<T, String> toString, String separator) {
		return blocks.stream().map(toString).collect(Collectors.joining(separator));
	}
	
	/**
	 * Encodes the provided {@code String}, escaping all unicode characters in {@code \\u} notation.
	 *
	 * @param s the {@code String} to be encoded
	 * @return the encoded {@code String}
	 */
	static String encode(String s) {
		StringBuilder sb = new StringBuilder();
		char[] charArray = s.toCharArray();
		for (char c : charArray) {
			if (c < 128) {
				sb.append(c);
			} else {
				sb.append("\\u").append(String.format("%04x", (int) c));
			}
		}
		return sb.toString();
	}
}