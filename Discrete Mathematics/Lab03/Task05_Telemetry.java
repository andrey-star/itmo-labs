import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

public class Task05_Telemetry {
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("telemetry.in"));
		int n = in.nextInt();
		int k = in.nextInt();
		in.close();
		PrintWriter out = new PrintWriter(new File("telemetry.out"));
		int total = (int) Math.pow(k, n);
		for (int it = 0; it < total; it++) {
			for (int j = 0; j < n; j++) {
				int kToj = (int) Math.pow(k, j);
				if (it % (2*kToj*k) < kToj*k) {
					out.print((it % kToj*k)/kToj);
				} else {
					out.print(k - 1 - (kToj*k)/kToj);
				}
			}
			out.println();
		}
		out.close();
	}
	
}
