import java.io.*;
import java.util.Scanner;

public class B_BinSearch {
	
	private static int binarySearch(int[] a, int key) {
		return binarySearch(a, key, 0, a.length - 1);
	}
	
	private static int binarySearch(int[] a, int key, int left, int right) {
		while (left <= right) {
			int mid = (left + right) / 2;
			int val = a[mid];
			if (val < key) {
				left = mid + 1;
			} else if (val > key) {
				right = mid - 1;
			} else {
				return mid;
			}
		}
		return -1;
	}
	
	private static int findRightBound(int[] a, int key, int left, int right) {
		while (left <= right) {
			int mid = (left + right) / 2;
			int val = a[mid];
			if (val == key) {
				left = mid + 1;
			} else {
				right = mid - 1;
			}
		}
		return right;
	}
	
	private static int findLeftBound(int[] a, int key, int left, int right) {
		while (left <= right) {
			int mid = (left + right) / 2;
			int val = a[mid];
			if (val == key) {
				right = mid - 1;
			} else {
				left = mid + 1;
			}
		}
		return left;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("binsearch.in")));
		int n = Integer.parseInt(in.readLine());
		int[] a = new int[n];
		String[] line = in.readLine().trim().split(" ");
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		PrintWriter out = new PrintWriter(new File("binsearch.out"));
		int searches = Integer.parseInt(in.readLine());
		line = in.readLine().trim().split(" ");
		for (int i = 0; i < searches; i++) {
			int s = Integer.parseInt(line[i]);
			int pos = binarySearch(a, s);
			if (pos != -1) {
				out.println(findLeftBound(a, s, 0, pos) + 1 + " " + (findRightBound(a, s, pos, a.length - 1) + 1));
			} else {
				out.println("-1 -1");
			}
		}
		out.close();
	}
	
}
