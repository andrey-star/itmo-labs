import java.io.*;

public class J_BigSegments {
	
	private static int a;
	private static int b;
	private static long cur = 0;
	
	private static long nextRand24() {
		cur = (cur * a + b) & ((1L << 32) - 1);
		return (cur >> 8);
	}
	
	private static int nextRand32() {
		long a = nextRand24();
		long b = nextRand24();
		return (int) (((a << 8) ^ b) & ((1L << 32) - 1));
		
	}
	
	private static long inversions = 0;
	
	private static void merge(long[] a, int leftStart, int mid, int rightEnd, long k) {
		int i = 0;
		int j = 0;
		int leftLength = mid - leftStart;
		int rightLength = rightEnd - mid;
		while (i < leftLength || j < rightLength) {
			if (i == leftLength) {
				while (j < rightLength) {
					inversions += leftLength - i;
					j++;
				}
				break;
			} else if (j == rightLength) {
				while (i < leftLength) {
					i++;
				}
				break;
			}
			if (a[i + leftStart] < a[j + mid] + k) {
				i++;
			} else {
				inversions += leftLength - i;
				j++;
			}
		}
		i = 0;
		j = 0;
		long[] buffer = new long[rightEnd - leftStart];
		while (i < leftLength || j < rightLength) {
			if (i == leftLength) {
				while (j < rightLength) {
					buffer[i + j] = a[j + mid];
					j++;
				}
				break;
			} else if (j == rightLength) {
				while (i < leftLength) {
					buffer[i + j] = a[i + leftStart];
					i++;
				}
				break;
			}
			if (a[i + leftStart] <= a[j + mid]) {
				buffer[i + j] = a[i + leftStart];
				i++;
			} else {
				buffer[i + j] = a[j + mid];
				j++;
			}
		}
		System.arraycopy(buffer, 0, a, leftStart, buffer.length);
	}
	
	private static long countInv(long[] a, long k) {
		sort(a, 0, a.length, k);
		return inversions;
	}
	
	private static void sort(long[] a, int left, int right, long k) {
		int aLength = right - left;
		if (aLength == 1) {
			if (a[left] >= k) {
				inversions++;
			}
			return;
		}
		int mid = aLength / 2;
		sort(a, left, mid + left, k);
		sort(a, mid + left, right, k);
		merge(a, left, mid + left, right, k);
	}
	
	private static void swap(long[] a, int i, int j) {
		long temp = a[i];
		a[i] = a[j];
		a[j] = temp;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("bigseg.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		long k = Long.parseLong(line[1]);
		line = in.readLine().trim().split(" +");
		a = Integer.parseInt(line[0]);
		b = Integer.parseInt(line[1]);
		in.close();
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = nextRand32();
		}
		long[] pref = new long[a.length];
		pref[0] = a[0];
		for (int i = 1; i < pref.length; i++) {
			pref[i] = pref[i - 1] + a[i];
		}
		for (int i = 0; i < pref.length / 2; i++) {
			swap(pref, i, pref.length - i - 1);
		}
		PrintWriter out = new PrintWriter(new File("bigseg.out"));
		out.println(countInv(pref, k));
		out.close();
	}
	
}
