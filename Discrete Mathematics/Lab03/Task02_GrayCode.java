import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Task02_GrayCode {
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("gray.in"));
		int n = in.nextInt();
		in.close();
		PrintWriter out = new PrintWriter(new File("gray.out"));
		for (int i = 0; i < (1 << n); i++) {
			for (int j = 0; j < n; j++) {
				out.print(((i^(i/2)) >> (n - j - 1)) & 1);
			}
			out.println();
		}
		out.close();
		
	}
	
}
