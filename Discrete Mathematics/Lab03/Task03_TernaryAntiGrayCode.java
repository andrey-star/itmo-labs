import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Task03_TernaryAntiGrayCode {
	
	private static String shift(String s, int n) {
		StringBuilder res = new StringBuilder();
		for (int i = 0; i < s.length(); i++) {
			res.append(((Integer.parseInt("" + s.charAt(i))) + n) % 3);
		}
		return res.toString();
	}
	
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("antigray.in"));
		int n = in.nextInt();
		in.close();
		PrintWriter out = new PrintWriter(new File("antigray.out"));
		int pow = (int) Math.pow(3, n - 1);
		for (int i = 0; i < pow; i++) {
			String s = "00000000000" + Integer.toString(i, 3);
			s = s.substring(s.length() - n);
			out.println(s);
			out.println(shift(s, 1));
			out.println(shift(s, 2));
		}
		out.close();
		
	}
	
}
