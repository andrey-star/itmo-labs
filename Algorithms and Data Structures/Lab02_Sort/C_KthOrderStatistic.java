import java.io.*;
import java.util.Random;

public class C_KthOrderStatistic {
	
	private static int findKthOrder(int[] a, int k, int left, int right) {
		Random random = new Random();
		int div = left + random.nextInt(right - left - 1);
		int pivot = a[div];
		int i = left;
		int j = right - 1;
		while (i < j) {
			while (a[i] < pivot) {
				i++;
			}
			while (a[j] > pivot) {
				j--;
			}
			if (i >= j) {
				break;
			}
			
			int temp = a[i];
			a[i++] = a[j];
			a[j--] = temp;
		}
		
		int leftBound = -1;
		int rightBound = right;
		for (i = left; i < right; i++) {
			if (a[i] < pivot) {
				leftBound = i;
			}
			if (a[i] > pivot) {
				rightBound = i;
				break;
			}
		}
		
		if (k <= leftBound - left) {
			right = leftBound + 1;
			return findKthOrder(a, k, left, right);
		}
		if (k >= rightBound - left) {
			k = k - rightBound + left;
			left = rightBound;
			return findKthOrder(a, k, left, right);
		}
		return pivot;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("kth.in")));
		PrintWriter out = new PrintWriter(new File("kth.out"));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		line = in.readLine().split(" ");
		int a = Integer.parseInt(line[0]);
		int b = Integer.parseInt(line[1]);
		int c = Integer.parseInt(line[2]);
		int[] num = new int[n];
		num[0] = Integer.parseInt(line[3]);
		if (n == 1) {
			out.println(num[0]);
		} else {
			num[1] = Integer.parseInt(line[4]);
			for (int i = 2; i < n; i++) {
				num[i] = a * num[i - 2] + b * num[i - 1] + c;
			}
			out.println(findKthOrder(num, k - 1,0, n));
		}
		out.close();
	}
	
}