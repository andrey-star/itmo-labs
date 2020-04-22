import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class AB_SubstringSearch {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String p = in.readLine();
		String t = in.readLine();
		in.close();
		
		String s = p + "$" + t;
		int n = s.length();
		int[] pref = new int[n];
		for (int i = 1; i < n; i++) {
			int j = pref[i - 1];
			while (j > 0 && s.charAt(i) != s.charAt(j)) {
				j = pref[j - 1];
			}
			if (s.charAt(i) == s.charAt(j)) {
				j++;
			}
			pref[i] = j;
		}
		List<Integer> res = new ArrayList<>();
		for (int i = p.length() + 1; i < n; i++) {
			if (pref[i] == p.length()) {
				res.add(i - 2 * p.length());
			}
		}
		System.out.println(res.size());
		res.forEach(r -> System.out.print(r + 1 + " "));
	}
	
}
