import java.io.*;

public class E_RadixSort {
	
	
	
	private static void radixIter(String[] a, int index) {
		if (index >= 0) {
			int n = a.length;
			String[] applied = new String[n];
			int[] freq = new int[27];
			for (String word : a) {
				freq[word.charAt(index) - 'a' + 1]++;
			}
			for (int i = 0; i < 26; i++) {
				freq[i + 1] += freq[i];
			}
			for (String word : a) {
				applied[freq[word.charAt(index) - 'a']++] = word;
			}
			System.arraycopy(applied, 0, a, 0, n);
		}
	
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("radixsort.in")));
		String[] firstLineOfFile = in.readLine().trim().split(" ");
		int n = Integer.parseInt(firstLineOfFile[0]);
		int m = Integer.parseInt(firstLineOfFile[1]);
		int k = Integer.parseInt(firstLineOfFile[2]);
		String[] a = new String[n];
		for (int i = 0; i < n; i++) {
			a[i] = in.readLine();
		}
		in.close();
		for (int i = 0; i < k; i++) {
			radixIter(a, m - i - 1);
		}
		PrintWriter out = new PrintWriter(new File("radixsort.out"));
		for (String s : a) {
			out.println(s);
		}
		out.close();
	}
	
}
