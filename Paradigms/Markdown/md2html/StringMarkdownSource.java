package md2html;

public class StringMarkdownSource {
	
	private static char END = '\0';
	
	private int pos;
	private String data;
	
	public StringMarkdownSource(final String data) {
		this.data = data + END;
	}
	
	public char getChar() {
		return data.charAt(pos);
	}
	
	public String getData() {
		return data;
	}
	
	public void next() {
		pos++;
	}
	
	public char peekNextChar() {
		return data.charAt(pos + 1);
	}
	
	public boolean endNotReached() {
		return getChar() != END;
	}
	
}
