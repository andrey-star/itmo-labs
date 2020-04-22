import java.util.*;

public class G_SortLinear {
	
	private static final int base = 10;
	
	private static void countSort(int[] arr, int n, int div) {
		int[] stage = new int[n];
		int[] freq = new int[base];
		
		for (int i = 0; i < n; i++) {
			freq[(arr[i] / div) % base]++;
		}
		
		for (int i = 0; i < base - 1; i++) {
			freq[i + 1] += freq[i];
		}
		
		for (int i = n - 1; i >= 0; i--) {
			stage[freq[(arr[i] / div) % base] - 1] = arr[i];
			freq[(arr[i] / div) % base]--;
		}
		
		for (int i = 0; i < n; i++) {
			arr[i] = stage[i];
		}
	}
	
	static void radixsort(int[] arr, int n) {
		int max = arr[0];
		for (int i = 1; i < n; i++) {
			if (arr[i] > max) {
				max = arr[i];
			}
		}
		int maxDigits = 0;
		for (int i = 0; i < max; i++) {
			if (max / ((int)Math.pow(base, i)) <= 0) {
				break;
			}
			maxDigits++;
		}
		System.out.println(max + " " + maxDigits);
		for (int i = 1; max/i > 0; i *= 10) {
			countSort(arr, n, i);
		}
	}
	
	
	public static void main(String[] args) {
		int[] a = {170, 45, 75, 90, 802, 24, 2, 66};
		int n = a.length;
		radixsort(a, n);
		System.out.println(Arrays.toString(a));
	}
	
	
}
