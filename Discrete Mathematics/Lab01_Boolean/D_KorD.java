import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class D_KorD {
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		long[] a = new long[n];
		String[] line = in.readLine().trim().split(" +");
		for (int i = 0; i < a.length; i++) {
			a[i] = Long.parseLong(line[i]);
		}
		long s = Long.parseLong(in.readLine());
		in.close();
		if (s == 0) {
			if (a[0] == 0) {
				System.out.println("(1)");
			} else {
				System.out.println(("(1&~1)"));
			}
		} else {
			int[] bits = new int[Long.toBinaryString(s).length()];
			int ones = 0;
			for (int i = 0; i < bits.length; i++) {
				bits[i] = (int) ((s >> i) & 1);
				if (bits[i] == 1) {
					ones++;
				}
			}
			int[][] form = new int[ones][n];
			int curClause = 0;
			for (int i = 0; i < bits.length; i++) {
				if (bits[i] == 1) {
					for (int j = 0; j < n; j++) {
						if (((a[j] >> i) & 1) == 1) {
							form[curClause][j] = 1;
						} else {
							form[curClause][j] = 0;
						}
					}
					curClause++;
				}
			}
			
			StringBuilder sdnf = new StringBuilder();
			sdnf.append("(");
			long result = 0;
			for (int i = 0; i < form.length; i++) {
				long clause = 0;
				for (int j = 0; j < n; j++) {
					if (form[i][j] == 1) {
						if (j == 0) {
							clause = a[j];
						} else {
							clause &= a[j];
						}
						sdnf.append(j + 1);
					} else {
						if (j == 0) {
							clause = ~a[j];
						} else {
							clause &= ~a[j];
						}
						sdnf.append("~").append(j + 1);
					}
					if (j < n - 1) {
						sdnf.append("&");
					}
				}
				if (i < form.length - 1) {
					sdnf.append(")|(");
				}
				result |= clause;
			}
			sdnf.append(")");
			if (result == s) {
				System.out.println(sdnf.toString());
			} else {
				System.out.println("Impossible");
			}
		}
	}
	
}
