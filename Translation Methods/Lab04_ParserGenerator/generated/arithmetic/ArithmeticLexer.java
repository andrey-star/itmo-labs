package arithmetic;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArithmeticLexer implements Iterator<ArithmeticToken>, Iterable<ArithmeticToken> {
	
	private final Map<ArithmeticToken.Type, Pattern> tokenPatterns = Map.ofEntries(
		Map.entry(ArithmeticToken.Type.PLUS, Pattern.compile("\\+")),
		Map.entry(ArithmeticToken.Type.MINUS, Pattern.compile("-")),
		Map.entry(ArithmeticToken.Type.MUL, Pattern.compile("\\*")),
		Map.entry(ArithmeticToken.Type.DIV, Pattern.compile("/")),
		Map.entry(ArithmeticToken.Type.LPAREN, Pattern.compile("\\(")),
		Map.entry(ArithmeticToken.Type.RPAREN, Pattern.compile("\\)")),
		Map.entry(ArithmeticToken.Type.COMMA, Pattern.compile(",")),
		Map.entry(ArithmeticToken.Type.NUM, Pattern.compile("[0-9]+")),
		Map.entry(ArithmeticToken.Type.IDENT, Pattern.compile("[a-zA-Z_][a-zA-Z0-9_]*"))
	);
	private final Pattern skip = Pattern.compile("[ \t\r\n]+");
	private final Matcher matcher = skip.matcher("");
	
	private int curStart = 0;
	private int curEnd = 0;
	private boolean hasNext = true;
	
	private final String input;
	
	public ArithmeticLexer(String input) {
		this.input = input;
	}
	
	@Override
	public ArithmeticToken next() {
		curStart = curEnd;
		matcher.usePattern(skip);
		matcher.reset(input.substring(curStart));
		matchLookingAt();
		for (var tokenType : ArithmeticToken.Type.values()) {
			if (tokenType == ArithmeticToken.Type._END) {
				continue;
			}
			matcher.usePattern(tokenPatterns.get(tokenType));
			if (matchLookingAt()) {
				return new ArithmeticToken(tokenType, input.substring(curStart, curEnd));
			}
		}
		if (curEnd != input.length()) {
			throw new UnmatchedPatternException(input, curEnd);
		}
		hasNext = false;
		return new ArithmeticToken(ArithmeticToken.Type._END, null);
	}
	
	@Override
	public boolean hasNext() {
		return hasNext;
	}
	
	@Override
	public Iterator<ArithmeticToken> iterator() {
		return this;
	}
	
	private boolean matchLookingAt() {
		if (matcher.lookingAt()) {
			curStart = curEnd;
			curEnd = curStart + matcher.end();
			matcher.reset(input.substring(curEnd));
			return true;
		}
		return false;
	}
	
	public int curPos() {
		return curEnd;
	}
	
	public static class UnmatchedPatternException extends RuntimeException {
		public UnmatchedPatternException(String text, int errIndex) {
			super("Unmatched pattern in text: '" + text + "' at index " + (errIndex + 1));
		}
	}
	
}