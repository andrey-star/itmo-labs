import java.io.*;

public class A_MergeSort {
	
	private static void merge(int[] a, int[] left, int[] right) {
		int i = 0;
		int j = 0;
		while (i < left.length || j < right.length) {
			if (i == left.length) {
				while (j < right.length) {
					a[i + j] = right[j];
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
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("sort.in")));
		int n = Integer.parseInt(in.readLine());
		String[] line = in.readLine().trim().split(" ");
//		int[] numbers = Arrays.stream(line).mapToInt(Integer::parseInt).toArray();
		int[] numbers = new int[n];
		for (int i = 0; i < n; i++) {
			numbers[i] = Integer.parseInt(line[i]);
		}
		sort(numbers);
		PrintWriter out = new PrintWriter(new File("sort.out"));
		for (int number : numbers) {
			out.print(number + " ");
		}
		out.close();
	}
	
}
