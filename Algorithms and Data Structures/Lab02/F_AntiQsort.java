import java.io.*;

public class F_AntiQsort {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("antiqs.in")));
		int n = Integer.parseInt(in.readLine());
		in.close();
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = i + 1;
		}
		for (int i = 0; i < n; i++) {
			int temp = a[i];
			a[i] = a[i/2];
			a[i/2] = temp;
		}
		PrintWriter out = new PrintWriter(new File("antiqs.out"));
		for (int i = 0; i < n; i++) {
			out.print(a[i] + " ");
		}
		out.close();
	}
	
}
