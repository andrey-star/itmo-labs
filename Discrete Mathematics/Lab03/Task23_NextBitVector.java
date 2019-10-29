import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Scanner;

public class Task23_NextBitVector {
	
	private static String plusOne(String s) {
		char[] c = s.toCharArray();
		if (!s.contains("0")) {
			return "-";
		} else {
			int i = s.length() - 1;
			while (c[i] == '1') {
				c[i] ='0';
				i--;
			}
			c[i] = '1';
		}
		return new String(c);
	}
	
	private static String minusOne(String s) {
		char[] c = s.toCharArray();
		if (!s.contains("1")) {
			return "-";
		} else {
			int i = s.length() - 1;
			while (c[i] == '0') {
				c[i] = '1';
				i--;
			}
			c[i] = '0';
		}
		return new String(c);
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("nextvector.in"));
		String n = in.next();
		String pred = minusOne(n);
		String next = plusOne(n);
		PrintWriter out = new PrintWriter(new File("nextvector.out"));
		out.println(pred + "\n" + next);
		out.close();
	}
	
}