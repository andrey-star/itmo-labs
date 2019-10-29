import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Task27_NextBracketSequence {
	private static String gen(int n, String s) {
		int open = 0;
		int close = 0;
		for (int i = s.length() - 1; i >= 0; i--) {
			char c = s.charAt(i);
			if (c == ')') {
				close++;
				continue;
			} else {
				open++;
			}
			if (close > open) {
				break;
			}
		}
		s = s.substring(0, s.length() - open - close);
		if (!s.isEmpty()) {
			s += ')';
			StringBuilder sb = new StringBuilder(s);
			for (int i = 0; i < (open + close - 1); i++) {
				if (i < open) {
					sb.append("(");
				} else {
					sb.append(")");
				}
			}
			s = sb.toString();
			return s;
		}
		return "-";
		
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("nextbrackets.in"));
		String seq = in.next();
		int n = seq.length() / 2;
		in.close();
		PrintWriter out = new PrintWriter(new File("nextbrackets.out"));
		out.println(gen(n, seq));
		out.close();
	}
	
}
