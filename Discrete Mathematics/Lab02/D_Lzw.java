import java.io.*;
import java.util.HashMap;

public class D_Lzw {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("lzw.in")));
		String s = in.readLine();
		in.close();
		HashMap<String, Integer> codes = new HashMap<>();
		String buf = "";
		int curCode = 0;
		for (char c = 'a'; c <= 'z'; c++) {
			codes.put(c + "", curCode++);
		}
		PrintWriter out = new PrintWriter(new File("lzw.out"));
		for (char c : s.toCharArray()) {
			if (codes.containsKey(buf + c)) {
				buf += c;
			} else {
				out.print(codes.get(buf) + " ");
				codes.put(buf + c, curCode++);
				buf = c + "";
			}
		}
		out.println(codes.get(buf));
		out.close();
	}
	
}
