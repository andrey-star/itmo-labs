import java.io.*;

public class H_InversionsCount {
	
	static long cur = 0;
	private static int nextRand24(int a, int b) {
		cur = (cur * a + b) & ((1L << 32) - 1);
		return (int) (cur >> 8);
	}
	static long inversions = 0;
	private static void merge(int[] a, int[] left, int[] right) {
		int i = 0;
		int j = 0;
		while (i < left.length || j < right.length) {
			if (i == left.length) {
				while (j < right.length) {
					a[i + j] = right[j];
					inversions += left.length - i;
					j++;
				}
				break;
			} else if (j == right.length) {
				while (i < left.length) {
					a[i + j] = left[i];
					i++;
				}
				break;
			}
			if (left[i] <= right[j]) {
				a[i + j] = left[i];
				i++;
			} else {
				a[i + j] = right[j];
				inversions += left.length - i;
				j++;
			}
		}
	}
	
	private static void sort(int[] a) {
		if (a.length == 1) {
			return;
		}
		int mid = a.length / 2;
		int[] left = new int[mid];
		System.arraycopy(a, 0, left, 0, left.length);
		int[] right = new int[a.length - mid];
		System.arraycopy(a, mid, right, 0, right.length);
		sort(left);
		sort(right);
		merge(a, left, right);
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("invcnt.in")));
		String[] line = in.readLine().trim().split(" ");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		line = in.readLine().trim().split(" ");
		int a = Integer.parseInt(line[0]);
		int b = Integer.parseInt(line[1]);
		int[] num = new int[n];
		for (int i = 0; i < n; i++) {
			num[i] = nextRand24(a, b) % m;
		}
		sort(num);
		PrintWriter out = new PrintWriter(new File("invcnt.out"));
		out.println(inversions);
		out.close();
	}
	
}
