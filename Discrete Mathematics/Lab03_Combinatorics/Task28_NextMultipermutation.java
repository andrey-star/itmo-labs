import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task28_NextMultipermutation {
	
	private static void nextMultipermutation(int[] a, int n) {
		for (int i = n - 1; i > 0; i--) {
			if (a[i - 1] < a[i]) {
				int minIndex = i;
				for (int j = i; j < n; j++) {
					if (a[j] > a[i - 1]) {
						minIndex = j;
					}
				}
				int temp = a[minIndex];
				a[minIndex] = a[i - 1];
				a[i - 1] = temp;
				for (int j = i; j < (n + i) / 2; j++) {
					int temp2 = a[j];
					a[j] = a[i + n - j - 1];
					a[i + n - j - 1] = temp2;
				}
				return;
			}
		}
		Arrays.fill(a, 0);
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("nextmultiperm.in"));
		int n = in.nextInt();
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = in.nextInt();
		}
		PrintWriter out = new PrintWriter(new File("nextmultiperm.out"));
		nextMultipermutation(a, n);
		Arrays.stream(a).forEach(i -> out.print(i + " "));
		out.close();
	}
	
}