import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class F_Period {
	
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
		int res = n;
		if (n % (n - pref[n - 1]) == 0) {
			res = n - pref[n - 1];
		}
		System.out.println(res);
	}
	
}
