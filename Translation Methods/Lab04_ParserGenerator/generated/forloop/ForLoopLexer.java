package forloop;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ForLoopLexer implements Iterator<ForLoopToken>, Iterable<ForLoopToken> {
	
	private final Map<ForLoopToken.Type, Pattern> tokenPatterns = Map.ofEntries(
		Map.entry(ForLoopToken.Type.FOR, Pattern.compile("for")),
		Map.entry(ForLoopToken.Type.EQ, Pattern.compile("==")),
		Map.entry(ForLoopToken.Type.INC, Pattern.compile("\\+\\+")),
		Map.entry(ForLoopToken.Type.DEC, Pattern.compile("--")),
		Map.entry(ForLoopToken.Type.LE, Pattern.compile("<=")),
		Map.entry(ForLoopToken.Type.GE, Pattern.compile(">=")),
		Map.entry(ForLoopToken.Type.GT, Pattern.compile(">")),
		Map.entry(ForLoopToken.Type.LT, Pattern.compile("<")),
		Map.entry(ForLoopToken.Type.LPAREN, Pattern.compile("\\(")),
		Map.entry(ForLoopToken.Type.RPAREN, Pattern.compile("\\)")),
		Map.entry(ForLoopToken.Type.SEMICOLON, Pattern.compile(";")),
		Map.entry(ForLoopToken.Type.ASSIGN, Pattern.compile("=")),
		Map.entry(ForLoopToken.Type.VAR, Pattern.compile("[a-zA-Z_][a-zA-Z0-9_]*")),
		Map.entry(ForLoopToken.Type.NUM, Pattern.compile("(\\+|-)?[0-9]+"))
	);
	private final Pattern skip = Pattern.compile("[ \t\r\n]+");
	private final Matcher matcher = skip.matcher("");
	
	private int curStart = 0;
	private int curEnd = 0;
	private boolean hasNext = true;
	
	private final String input;
	
	public ForLoopLexer(String input) {
		this.input = input;
	}
	
	@Override
	public ForLoopToken next() {
		curStart = curEnd;
		matcher.usePattern(skip);
		matcher.reset(input.substring(curStart));
		matchLookingAt();
		for (var tokenType : ForLoopToken.Type.values()) {
			if (tokenType == ForLoopToken.Type._END) {
				continue;
			}
			matcher.usePattern(tokenPatterns.get(tokenType));
			if (matchLookingAt()) {
				return new ForLoopToken(tokenType, input.substring(curStart, curEnd));
			}
		}
		if (curEnd != input.length()) {
			throw new UnmatchedPatternException(input, curEnd);
		}
		hasNext = false;
		return new ForLoopToken(ForLoopToken.Type._END, null);
	}
	
	@Override
	public boolean hasNext() {
		return hasNext;
	}
	
	@Override
	public Iterator<ForLoopToken> iterator() {
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