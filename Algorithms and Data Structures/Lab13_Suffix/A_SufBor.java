import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

public class A_SufBor {
	
	private static final int MAX_STATES = 100 * 100 * 2;
	private static int states = 1;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s = in.readLine();
		in.close();
		List<Triple> res = new ArrayList<>();
		Node[] bor = new Node[MAX_STATES];
		bor[0] = new Node();
		for (int i = s.length() - 1; i >= 0; i--) {
			addWord(bor, s.substring(i), res);
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println(states + " " + res.size());
		res.forEach(t -> out.println(t.a + 1 + " " + (t.b + 1) + " " + t.c));
		out.close();
	}
	
	private static void addWord(Node[] bor, String word, List<Triple> res) {
		final int[] u = {0};
		word.chars().forEach(c -> {
			c = c - 'a';
			if (bor[u[0]].ch[c] == 0) {
				bor[states] = new Node();
				bor[u[0]].ch[c] = states;
				res.add(new Triple(u[0], states, (char) (c + 'a')));
				states++;
			}
			u[0] = bor[u[0]].ch[c];
		});
	}
	
	private static class Node {
		int[] ch = new int[26];
	}
	
	private static class Triple {
		int a;
		int b;
		char c;
		
		public Triple(int a, int b, char c) {
			this.a = a;
			this.b = b;
			this.c = c;
		}
	}
}
