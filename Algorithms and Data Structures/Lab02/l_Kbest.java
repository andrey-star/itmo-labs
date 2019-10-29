import java.io.*;
import java.util.Random;

public class l_Kbest {
	
	private static void findKthOrder(Jewel[] a, int k, int left, int right, double m) {
		if (right == left + 1) {
			return;
		}
		Random random = new Random();
		int div = left + random.nextInt(right - left - 1);
		double pivot = rate(a[div], m);
		int i = left;
		int j = right - 1;
		while (i < j) {
			while (rate(a[i], m) < pivot) {
				i++;
			}
			while (rate(a[j], m) > pivot) {
				j--;
			}
			if (i >= j) {
				break;
			}
			
			Jewel temp = a[i];
			a[i++] = a[j];
			a[j--] = temp;
		}
		
		int leftBound = -1;
		int rightBound = right;
		for (i = left; i < right; ++i) {
			if (rate(a[i], m) < pivot) {
				leftBound = i;
			}
			if (rate(a[i], m) > pivot) {
				rightBound = i;
				break;
			}
		}
		
		if (k <= leftBound - left) {
			right = leftBound + 1;
			findKthOrder(a, k, left, right, m);
		}
		if (k >= rightBound - left) {
			k = k - rightBound + left;
			left = rightBound;
			findKthOrder(a, k, left, right, m);
		}
	}
	
	static class Jewel {
		int cost;
		int weight;
		int index;
		
		Jewel(int cost, int weight, int i) {
			this.cost = cost;
			this.weight = weight;
			index = i;
		}
	}
	
	private static double rate(Jewel j, double av) {
		return j.cost - av*j.weight;
	}
	
	private static void binSearch(Jewel[] jewels, int k) {
		double left = 0;
		double right = Integer.MAX_VALUE;
		for (int i = 0; i < 100; i++) {
			double m = left + (right - left)/2;
			findKthOrder(jewels, jewels.length - k, 0, jewels.length, m);
			double sum = 0;
			for (int j = 0; j < k; j++) {
				sum += rate(jewels[jewels.length - j - 1], m);
			}
			if (sum >= 0) {
				left = m;
			} else {
				right = m;
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("kbest.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		Jewel[] jewels = new Jewel[n];
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int cost = Integer.parseInt(line[0]);
			int weight = Integer.parseInt(line[1]);
			jewels[i] = new Jewel(cost, weight, i);
		}
		binSearch(jewels, k);
		PrintWriter out = new PrintWriter(new File("kbest.out"));
		for (int i = 0; i < k; i++) {
			out.print(jewels[n - i - 1].index + 1 + " ");
		}
		out.close();
	}
	
}
