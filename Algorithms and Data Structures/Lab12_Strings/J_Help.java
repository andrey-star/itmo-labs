import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class J_Help {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s = in.readLine();
		in.close();
		
		int n = s.length();
		int[] res = new int[n];
		res[0] = 1;
		int[] z = new int[n];
		StringBuilder sb = new StringBuilder();
		sb.append(s.charAt(0));
		for (int i = 1; i < n; i++) {
			sb.append(s.charAt(i));
			sb.reverse();
			z(z, sb);
			res[i] = res[i - 1] + sb.length() - max(z, sb.length());
			sb.reverse();
		}
		for (int re : res) {
			System.out.println(re);
		}
	}
	
	private static int max(int[] z, int prefixSize) {
		int res = Integer.MIN_VALUE;
		for (int i = 0; i < prefixSize; i++) {
			res = Math.max(res, z[i]);
		}
		return res;
	}
	
	private static void z(int[] z, StringBuilder s) {
		int n = s.length();
		Arrays.fill(z, 0);
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
	}
	
}
