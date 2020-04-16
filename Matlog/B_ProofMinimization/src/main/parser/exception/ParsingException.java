package main.parser.exception;

public class ParsingException extends Exception {
	
	private final int index;
	
	public ParsingException(String format, int index, Object... args) {
		super(String.format(format, args) + " at index " + (index + 1));
		this.index = index;
	}
	
	public int getIndex() {
		return index + 1;
	}
}
