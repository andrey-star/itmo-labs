import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Task09_AllBracketSequences {
	private static PrintWriter out;
	private static void gen(String p, int n, int open, int close) {
		if (open + close == 2*n) {
			p.chars().forEach(c -> out.print((char) c));
			out.println();
			return;
		}
		if (open < n) {
			gen(p + "(", n, open + 1, close);
		}
		if (close < open) {
			gen(p + ")", n, open, close + 1);
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("brackets.in"));
		int n = in.nextInt();
		in.close();
		out = new PrintWriter(new File("brackets.out"));
		gen("", n, 0, 0);
		out.close();
	}
	
}
