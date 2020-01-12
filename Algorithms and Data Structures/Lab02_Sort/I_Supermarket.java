import java.io.*;
import java.util.Random;

public class I_Supermarket {
	
	private static int[] boxesA;
	private static int[] boxesB;
	private static int[] boxesT;
	private static int m;
	
	static long out(int i, long time) {
		time -= boxesT[i] + boxesB[i];
		int a = boxesA[i];
		if (a != 0) {
			return Math.max(time / a, 0);
		}
		return (int) 1e5 + 20;
	}
	
	private static long maxTimeToManage(int prod) {
		return ((long) boxesT[0]) + boxesB[0] + ((long) boxesA[0]) * prod;
	}
	
	private static void findKthOrder(int k, long time) {
		int left = 0;
		int right = m;
		while (right != left + 1) {
			Random random = new Random();
			int div = left + random.nextInt(right - left - 1);
			long pivot = out(div, time);
			int i = left;
			int j = right - 1;
			while (i < j) {
				while (out(i, time) < pivot) {
					i++;
				}
				while (out(j, time) > pivot) {
					j--;
				}
				if (i >= j) {
					break;
				}
				
				int tempA = boxesA[i];
				boxesA[i] = boxesA[j];
				boxesA[j] = tempA;
				int tempB = boxesB[i];
				boxesB[i] = boxesB[j];
				boxesB[j] = tempB;
				int tempT = boxesT[i];
				boxesT[i] = boxesT[j];
				boxesT[j] = tempT;
				i++;
				j--;
			}
			
			int leftBound = -1;
			int rightBound = right;
			for (i = left; i < right; i++) {
				if (out(i, time) < pivot) {
					leftBound = i;
				}
				if (out(i, time) > pivot) {
					rightBound = i;
					break;
				}
			}
			
			if (k <= leftBound - left) {
				right = leftBound + 1;
				continue;
			}
			if (k >= rightBound - left) {
				k = k - rightBound + left;
				left = rightBound;
				continue;
			}
			return;
		}
	}
	
	private static long binSearch(int n, int p) {
		long left = 0;
		long right = maxTimeToManage(p);
		long time;
		while (left < right) {
			time = left + (right - left) / 2;
			if (m - n > 0) {
				findKthOrder(m - n, time);
			}
			long sum = 0;
			for (int j = 0; j < Math.min(n, m); j++) {
				sum += out(m - j - 1, time);
			}
			if (sum >= p) {
				right = time;
			} else {
				left = time + 1;
			}
		}
		return left;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("supermarket.in")));
		m = Integer.parseInt(in.readLine());
		boxesA = new int[m];
		boxesB = new int[m];
		boxesT = new int[m];
		for (int i = 0; i < m; i++) {
			String[] line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			int t = Integer.parseInt(line[2]);
			boxesA[i] = a;
			boxesB[i] = b;
			boxesT[i] = t;
			
		}
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int p = Integer.parseInt(line[1]);
		PrintWriter out = new PrintWriter(new File("supermarket.out"));
		out.println(binSearch(n, p));
		out.close();
	}
	
}
