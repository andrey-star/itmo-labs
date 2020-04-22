import java.io.*;
import java.util.LinkedList;

public class R_Number {
	
	static class State {
		int digitsSum;
		int modN;
		StringBuilder number;
		
		State(int digitsSum, int modN) {
			this.digitsSum = digitsSum;
			this.modN = modN;
			number = new StringBuilder();
		}
		
	}
	private static String bfs(State s, int n) {
		LinkedList<State> queue = new LinkedList<>();
		queue.push(s);
		boolean[][] used = new boolean[n + 1][n];
		used[s.digitsSum][s.modN] = true;
		State u;
		do {
			u = queue.getFirst();
			queue.pop();
			for (int i = 0; i < 10; i++) {
				int a = u.digitsSum + i;
				int b = (u.modN * 10 + i) % n;
				State v = new State(a, b);
				v.number = new StringBuilder(u.number);
				v.number.append(i);
				if (a <= n && !used[v.digitsSum][v.modN]) {
					used[v.digitsSum][v.modN] = true;
					queue.add(v);
				}
			}
		} while (u.digitsSum != n || u.modN != 0);
		return u.number.toString();
	}
	
	
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("number.in")));
		int n = Integer.parseInt(in.readLine());
		in.close();
		PrintWriter out = new PrintWriter(new File("number.out"));
		out.println(bfs(new State(0, 0), n));
		out.close();
	}
	
}