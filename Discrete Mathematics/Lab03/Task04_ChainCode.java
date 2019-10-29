import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Scanner;

public class Task04_ChainCode {
	
	private static String zeros(int n) {
		StringBuilder res = new StringBuilder();
		for (int i = 0; i < n; i++) {
			res.append("0");
		}
		return res.toString();
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("chaincode.in"));
		int n = in.nextInt();
		in.close();
		String cur = zeros(n);
		HashSet<String> code = new HashSet<>();
		PrintWriter out = new PrintWriter(new File("chaincode.out"));
		while (true) {
			code.add(cur);
			out.println(cur);
			String next = cur.substring(1) + "1";
			if (!code.contains(next)) {
				cur = next;
				continue;
			}
			next = cur.substring(1) + "0";
			if (!code.contains(next)) {
				cur = next;
				continue;
			}
			break;
		}
		out.close();
		
	}
	
}
