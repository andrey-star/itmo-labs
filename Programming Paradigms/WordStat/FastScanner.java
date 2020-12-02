
import java.io.*;
import java.util.ArrayList;

public class FastScanner implements AutoCloseable {
	
	private final int BUFFER_SIZE = 1 << 10;
	private InputStream in;
	private byte[] buffer;
	private int curBuffer;
	private int foundBytes;
	private boolean skipLineSeparator;
	
	FastScanner(InputStream in) {
		this.in = in;
		buffer = new byte[BUFFER_SIZE];
		curBuffer = 0;
		foundBytes = 0;
		skipLineSeparator = (System.lineSeparator().length() == 2);
	}
	
	FastScanner(String source) {
		in = new ByteArrayInputStream(source.getBytes());
		buffer = new byte[BUFFER_SIZE];
		curBuffer = 0;
		foundBytes = 0;
		skipLineSeparator = (System.lineSeparator().length() == 2);
	}
	
	String readLine() throws IOException {
		StringBuilder chars = new StringBuilder();
		char c = nextChar();
		while (c != '\n' && c != '\r') {
			chars.append(c);
			if (!hasNextChar()) {
				break;
			}
			c = nextChar();
		}
		curBuffer++;
		if (curBuffer >= foundBytes) {
			fillBuffer();
		}
		if (buffer[curBuffer] == '\n' && skipLineSeparator) {
			curBuffer++;
		}
		return chars.toString();
	}
	
	String readWordLine() throws IOException {
		return toWord(readLine());
	}
	
	private char nextChar() throws IOException {
		byte first = nextByte();
		int amount = 1;
		while (((first >> (8 - amount)) & 1) == 1) {
			amount++;
		}
		amount--;
		int character = first & ((1 << (8 - amount)) - 1);
		for (int i = 0; i < amount - 1; i++) {
			character <<= 6;
			byte next = nextByte();
			character += next & 0b111111;
		}
		return (char) character;
	}
	
	private byte nextByte() throws IOException {
		if (curBuffer >= foundBytes) {
			fillBuffer();
		}
		if (foundBytes != -1) {
			return buffer[curBuffer++];
		} else {
			return -1;
		}
	}
	
	private void fillBuffer() throws IOException {
		curBuffer = 0;
		do {
			foundBytes = in.read(buffer);
		} while (foundBytes == 0);
	}
	
	private boolean hasNextChar() throws IOException {
		if (curBuffer >= foundBytes) {
			fillBuffer();
		}
		return foundBytes != -1;
	}
	
	boolean hasNextLine() throws IOException {
		return hasNextChar();
	}
	
	private String toWord(String s) {
		StringBuilder sb = new StringBuilder();
		for (char c : s.toCharArray()) {
			if (Character.getType(c) == Character.DASH_PUNCTUATION || Character.isLetter(c) || c == '\'') {
				sb.append(c);
			} else {
				sb.append(" ");
			}
		}
		return sb.toString().toLowerCase().trim();
	}
	
	String[] split(String s) {
		int size = 0;
		int i = 0;
		while (i < s.length()) {
			while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
				i++;
			}
			if (i < s.length()) {
				size++;
				while (i < s.length() && !Character.isWhitespace(s.charAt(i))) {
					i++;
				}
			}
		}
		String[] words = new String[size];
		i = 0;
		int pos = 0;
		while (i < s.length()) {
			while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
				i++;
			}
			if (i < s.length()) {
				int start = i;
				while (i < s.length() && !Character.isWhitespace(s.charAt(i))) {
					i++;
				}
				words[pos] = s.substring(start, i);
				pos++;
			}
		}
		return words;
	}
	
	private char[] listToArray(ArrayList<Character> a) {
		char[] mas = new char[a.size()];
		for (int i = 0; i < a.size(); i++) {
			mas[i] = a.get(i);
		}
		return mas;
	}
	
	@Override
	public void close() throws IOException {
		if (in != null) {
			in.close();
		}
	}
	
	public static void main(String[] args) throws IOException {
		FastScanner in = new FastScanner(new FileInputStream("test.txt"));
		while (in.hasNextLine()) {
			System.out.println(in.readLine());
		}
		byte[] b = new byte[1];
		new String(b, 0, 1, "utf8");
	}
	
}