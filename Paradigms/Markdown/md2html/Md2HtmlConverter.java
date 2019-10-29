package md2html;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Md2HtmlConverter implements Converter {
	
	private final Pattern LINK_PATTERN = Pattern.compile("\\[([^]]+)]\\(([^)]*)\\)");
	private final StringMarkdownSource source;
	private final Map<String, String> tags;
	
	public Md2HtmlConverter(String text) {
//		System.err.println(text + "\n____________");
		this.source = new StringMarkdownSource(text
				.replaceAll("&", "&amp;")
				.replaceAll("<", "&lt;")
				.replaceAll(">", "&gt;"));
		tags = Map.of("*", "em",
				"**", "strong",
				"++", "u",
				"_", "em",
				"__", "strong",
				"--", "s",
				"`", "code",
				"~", "mark");
	}
	
	@Override
	public String convert() {
		StringBuilder result = new StringBuilder();
		while (source.endNotReached()) {
			// Check header
			int headerType = 0;
			StringBuilder skippedHeader = new StringBuilder();
			while (source.endNotReached() && test('#')) {
				headerType++;
				skippedHeader.append(source.getChar());
				source.next();
				if (headerType > 6) {
					headerType = 0;
					break;
				}
			}
			
			if (source.endNotReached() && headerType > 0 && test(' ')) { // if header confirmed
				source.next();
				result.append(wrapWithTag(parseBlock(), "h" + headerType)).append('\n');
			} else {
				String possibleEmpty = ""; // "\n" if line empty, the whitespace buffer otherwise
				if (Character.isWhitespace(source.getChar())) {
					possibleEmpty = checkEmptyLine();
				}
				if (!possibleEmpty.equals("\n")) { // if the line is not empty - parse the contents into a paragraph
					result.append(wrapWithTag(skippedHeader + possibleEmpty + parseBlock(), "p"))
							.append('\n');
				}
			}
		}
		if (result.length() > 0) { // delete last empty line
			result.deleteCharAt(result.length() - 1);
		}
		return result.toString();
	}
	
	private String wrapWithTag(String content, String tagName) {
		return "<" + tagName + ">" + content + "</" + tagName + ">";
	}
	
	private String parseBlock() {
		StringBuilder content = new StringBuilder();
		while (source.endNotReached()) {
			if (test('\n')) { // possibly empty line
				content.append(source.getChar());
				source.next();
				String possibleEmpty = checkEmptyLine();
				if (possibleEmpty.equals("\n") || !source.endNotReached()) { // if the line is actually empty
					// remove redundant whitespace at the end
					content.deleteCharAt(content.length() - 1);
					break;
				} else { // line wasn't empty
					content.append(possibleEmpty);
				}
			} else {
				content.append(source.getChar());
				source.next();
			}
		}
		return toHtml(content.toString());
	}
	
	private String toHtml(String content) {
		final char end = '\0';
		final char skip = '\1';
		
		// Replace links with their html, and mark them to be skipped
		Matcher matcher = LINK_PATTERN.matcher(content);
		StringBuffer sb = new StringBuffer();
		while (matcher.find()) {
			int prev = matcher.start() - 1;
			String replacement;
			boolean image = false;
			if (prev >= 0 && content.charAt(prev) == '!') {
				replacement = "<img alt='" + matcher.group(1) + "' src='" + matcher.group(2) + "'>";
				image = true;
			} else {
				replacement = "<a href='" + matcher.group(2) + "'>" + toHtml(matcher.group(1)) + "</a>";
			}
			matcher.appendReplacement(sb, skip + replacement + skip);
			if (image) {
				sb.deleteCharAt(sb.length() - replacement.length() - 3); // delete !
			}
		}
		matcher.appendTail(sb);
		content = sb.toString();
		
		content += end;
		int pointer = 0;
		Map<String, Pair> tagInfo = new HashMap<>();
		StringBuilder contentHtml = new StringBuilder();
		while (content.charAt(pointer) != end) {
			if (content.charAt(pointer) == skip) {
				int skipClosingIndex = content.indexOf(skip, pointer + 1);
				contentHtml.append(content, pointer + 1, skipClosingIndex);
				pointer = skipClosingIndex + 1;
			} else if (content.charAt(pointer) == '\\') { // escape
				if (content.charAt(pointer + 1) != end) {
					pointer++;
					contentHtml.append(pointer < content.length() ? content.charAt(pointer) : "");
				} else {
					contentHtml.append("\\");
				}
				pointer++;
			} else if (isNextSymbolSpecial(content, pointer)) {
				String symbol = nextSpecialSymbol(tagInfo, content, pointer);
				pointer += symbol.length();
				handleSpecialTag(symbol, contentHtml, tagInfo);
			} else {
				contentHtml.append(content.charAt(pointer));
				pointer++;
			}
		}
		
		// Remove unclosed tags
		var infoSorted = sortedByLastIndexOf(tagInfo);
		for (var entry : infoSorted) {
			if (entry.getValue().amount % 2 == 1) {
				int offset = entry.getValue().lastIndexOf;
				contentHtml.delete(offset, offset + tags.get(entry.getKey()).length() + 2);
				contentHtml.insert(offset, entry.getKey());
			}
		}
		return contentHtml.toString();
	}
	
	private void handleSpecialTag(String symbol, StringBuilder contentHtml, Map<String, Pair> tagInfo) {
		Pair symbolInfo = tagInfo.computeIfAbsent(symbol, k -> new Pair(0, 0));
		symbolInfo.amount++;
		symbolInfo.lastIndexOf = contentHtml.length();
		if (symbolInfo.amount % 2 == 1) {
			contentHtml.append("<");
		} else {
			contentHtml.append("</");
		}
		contentHtml.append(tags.get(symbol)).append(">");
	}
	
	private List<Map.Entry<String, Pair>> sortedByLastIndexOf(Map<String, Pair> map) {
		return map.entrySet().stream()
				.sorted(Comparator.comparingInt(o -> -o.getValue().getLastIndexOf()))
				.collect(Collectors.toList());
	}
	
	private String nextSpecialSymbol(Map<String, Pair> tagInfo, String content, int index) {
		char firstSymbol = content.charAt(index);
		if (content.charAt(index) == '\0' || content.charAt(index + 1) != firstSymbol || !tags.containsKey(firstSymbol + "" + firstSymbol)
				|| (tagInfo.containsKey("" + firstSymbol) && tagInfo.get("" + firstSymbol).amount % 2 == 1)) {
			return "" + firstSymbol;
		} else {
			return firstSymbol + "" + firstSymbol;
		}
	}
	
	private boolean isNextSymbolSpecial(String content, int index) {
		char firstSymbol = content.charAt(index);
		return (tags.containsKey("" + firstSymbol)) || (index < content.length() - 1 &&
				tags.containsKey(firstSymbol + "" + content.charAt(index + 1)));
	}
	
	// Checks whether the line starting from current char contains non whitespace characters
	// Leaves the cursor on the first non-whitespace character, if the line is not empty
	// and on the first character of the next line, if the line is empty
	private String checkEmptyLine() {
		StringBuilder whitespacePref = new StringBuilder();
		boolean isLineEmpty = false;
		while (source.endNotReached() && Character.isWhitespace(source.getChar())) {
			if (test('\n')) {
				isLineEmpty = true;
				source.next();
				break;
			}
			whitespacePref.append(source.getChar());
			source.next();
		}
		return isLineEmpty ? "\n" : whitespacePref.toString();
	}
	
	private boolean test(final char c) {
		return source.getChar() == c;
	}
}
