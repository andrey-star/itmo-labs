import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task25_NextCombination {
	
	private static void nextCombination(int[] a, int n, int k) {
		int[] b = new int[k + 1];
		System.arraycopy(a, 0, b, 0, k);
		b[k] = n + 1;
		for (int i = k - 1; i >= 0; i--) {
			if (b[i + 1] - b[i] >= 2) {
				b[i]++;
				for (int j = i + 1; j <= k; j++) {
					b[j] = b[j - 1] + 1;
				}
				System.arraycopy(b, 0, a, 0, k);
				return;
			}
		}
		Arrays.fill(a, 0);
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("nextchoose.in"));
		int n = in.nextInt();
		int k = in.nextInt();
		int[] a = new int[k];
		for (int i = 0; i < k; i++) {
			a[i] = in.nextInt();
		}
		nextCombination(a, n, k);
		PrintWriter out = new PrintWriter(new File("nextchoose.out"));
		if (a[0] == 0) {
			out.println("-1");
		} else {
			Arrays.stream(a).forEach(i -> out.print(i + " "));
		}
		out.close();
	}
	
}