import java.io.*;
import java.util.Arrays;

public class B_BurrowsWiller {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("bwt.in")));
		String s = in.readLine();
		in.close();
		int n = s.length();
		String[] shift = new String[n];
		shift[0] = s;
		for (int i = 1; i < n; i++) {
			shift[i] = shift[i - 1].substring(1) + shift[i - 1].charAt(0);
		}
		Arrays.sort(shift);
		PrintWriter out = new PrintWriter(new File("bwt.out"));
		for (int i = 0; i < n; i++) {
			out.print(shift[i].charAt(n - 1));
		}
		out.close();
	}
	
}
