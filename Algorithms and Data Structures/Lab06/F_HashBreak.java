import java.io.*;

public class F_HashBreak {
	public static void main(String[] args) throws IOException {
		int n = 1024;
		int p = 32 - Integer.numberOfLeadingZeros(n - 1);
		PrintWriter out = new PrintWriter(new File("map.in"));
		int c = 0;
		for (int k = 'A'; k < 'z'; k++) {
			for (int q = 'A'; q < 'z'; q++) {
				for (int i = 0; i < n; i++) {
					StringBuilder s = new StringBuilder();
					for (int j = 0; j < p; j++) {
						if (((i >> j) & 1) == 1) {
							s.append((char) k).append((char) q);
						} else {
							s.append((char) (k + 1)).append((char) (q - 31));
						}
					}
					c++;
					out.println("put " + s + " 0");
				}
			}
		}
		out.close();
		System.out.println(c);
	}
}
