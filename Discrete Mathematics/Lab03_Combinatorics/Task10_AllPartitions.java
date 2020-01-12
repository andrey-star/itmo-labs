import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

public class Task10_AllPartitions {
	private static PrintWriter out;
	private static int n;
	private static void gen(int[] p, int target, int sum, int length) {
		if (sum == n) {
			for (int i = 0; i < length; i++) {
				out.print(p[i]);
				if (i != length - 1) {
					out.print("+");
				}
			}
			out.println();
		}
		int bound = 1;
		if (length > 0) {
			bound = p[length - 1];
		}
		for (int i = 1; i <= target; i++) {
			if (sum + i <= n && i >= bound) {
				p[length] = i;
				gen(p, target - 1, sum + i, length + 1);
			}
		}
		
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("partition.in"));
		n = in.nextInt();
		in.close();
		out = new PrintWriter(new File("partition.out"));
		gen(new int[n], n, 0, 0);
		out.close();
	}
	
}
