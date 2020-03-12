import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class C_PrefixFunction {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s = in.readLine();
		in.close();
		
		int n = s.length();
		int[] pref = new int[n];
		for (int i = 1; i < n; i++) {
			int k = pref[i - 1];
			while (k > 0 && s.charAt(i) != s.charAt(k)) {
				k = pref[k - 1];
			}
			if (s.charAt(i) == s.charAt(k)) {
				k++;
			}
			pref[i] = k;
		}
		PrintWriter out = new PrintWriter(System.out);
		for (int i = 0; i < n; i++) {
			out.print(pref[i] + " ");
		}
		out.close();
	}
	
}
