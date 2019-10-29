import java.io.*;

public class A_IsHeap {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("isheap.in")));
		int n = Integer.parseInt(in.readLine());
		String[] line = in.readLine().trim().split(" +");
		int[] a = new int[n];
		for (int i = 0; i < a.length; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		boolean isHeap = true;
		for (int i = 0; i < a.length; i++) {
			if (2*i + 1 < n && a[i] > a[2*i + 1]) {
				isHeap = false;
				break;
			}
			if (2*i + 2 < n && a[i] > a[2*i + 2]) {
				isHeap = false;
				break;
			}
		}
		PrintWriter out = new PrintWriter(new File("isheap.out"));
		out.println(isHeap ? "YES" : "NO");
		out.close();
	}
	
}
