import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task24_PreviousAndNextPermutation {
	
	private static void prevPermutation(int[] a, int n) {
		for (int i = n - 1; i > 0; i--) {
			if (a[i - 1] > a[i]) {
				int maxIndex = i;
				int maxBig = a[maxIndex];
				for (int j = i; j < n; j++) {
					if (a[j] > maxBig && a[j] < a[i - 1]) {
						maxBig = a[j];
						maxIndex = j;
					}
				}
				int temp = a[maxIndex];
				a[maxIndex] = a[i - 1];
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
	
	private static void nextPermutation(int[] a, int n) {
		for (int i = n - 1; i > 0; i--) {
			if (a[i - 1] < a[i]) {
				int minIndex = i;
				int minBig = a[minIndex];
				for (int j = i; j < n; j++) {
					if (a[j] < minBig && a[j] > a[i - 1]) {
						minBig = a[j];
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
		Scanner in = new Scanner(new File("nextperm.in"));
		int n = in.nextInt();
		int[] prev = new int[n];
		int[] next = new int[n];
		for (int i = 0; i < n; i++) {
			next[i] = in.nextInt();
			prev[i] = next[i];
		}
		PrintWriter out = new PrintWriter(new File("nextperm.out"));
		prevPermutation(prev, n);
		Arrays.stream(prev).forEach(i -> out.print(i + " "));
		out.println();
		nextPermutation(next, n);
		Arrays.stream(next).forEach(i -> out.print(i + " "));
		out.close();
	}
	
}