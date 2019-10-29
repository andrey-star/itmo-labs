import java.io.*;
import java.nio.charset.StandardCharsets;

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
		StringBuilder sb = new StringBuilder();
		int c;
		int startOfLine;
		while (true) {
			if (curBuffer >= foundBytes) {
				fillBuffer();
			}
			if (foundBytes == -1) {
				if (sb.length() > 0) {
					return sb.toString();
				}
				return null;
			}
			boolean eol = false;
			int i;
			for (i = curBuffer; i < foundBytes; i++) {
				c = buffer[i];
				if ((c == '\n') || (c == '\r')) {
//					if (c == '\r') {
//						skipLineSeparator = true;
//					}
					eol = true;
					break;
				}
			}
			startOfLine = curBuffer;
			curBuffer = i;
			sb.append(new String(buffer, startOfLine, curBuffer - startOfLine));
			if (eol) {
				curBuffer++;
				if (curBuffer >= foundBytes) {
					fillBuffer();
				}
				if (buffer[curBuffer] == '\n' && skipLineSeparator) {
					curBuffer++;
				}
				return sb.toString();
			}
		}
	}
	
	private void fillBuffer() throws IOException {
		curBuffer = 0;
		do {
			foundBytes = in.read(buffer);
		} while (foundBytes == 0);
	}
	
	boolean hasNextLine() throws IOException {
		if (curBuffer >= foundBytes) {
			fillBuffer();
		}
		return foundBytes != -1;
	}
	
	static String[] split(String s) {
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
	
	@Override
	public void close() throws IOException {
		if (in != null) {
			in.close();
		}
	}
	
	public static void main(String[] args) throws IOException {
		FastScanner in = new FastScanner(new FileInputStream("input.txt"));
		PrintWriter out = new PrintWriter(new File("output.txt"));
		System.out.println(in.readLine());
//		out.println(in.readLine());
		out.close();
	}
}
