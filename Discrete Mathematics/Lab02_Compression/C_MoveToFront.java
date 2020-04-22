import java.io.*;
import java.util.LinkedList;

public class C_MoveToFront {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("mtf.in")));
		String s = in.readLine();
		in.close();
		LinkedList<Character> a = new LinkedList<>();
		for (char i = 'a'; i <= 'z'; i++) {
			a.add(i);
		}
		PrintWriter out = new PrintWriter(new File("mtf.out"));
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			int index = a.indexOf(c);
			a.remove(index);
			a.addFirst(c);
			out.print(index + 1 + " ");
		}
		out.close();
	}
	
}
