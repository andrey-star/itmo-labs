import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class D_ZFunction {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s = in.readLine();
		in.close();
		int n = s.length();
		int[] z = new int[n];
		int l = 0;
		for (int i = 1; i < n; i++) {
			if (i <= l + z[l] - 1) {
				int j = i - l;
				z[i] = Math.min(z[l] - j, z[j]);
			}
			while (i + z[i] < n && s.charAt(z[i]) == s.charAt(i + z[i])) {
				z[i]++;
				l = i;
			}
		}
		PrintWriter out = new PrintWriter(System.out);
		for (int i = 1; i < n; i++) {
			out.print(z[i] + " ");
		}
		out.close();
	}
	
}
