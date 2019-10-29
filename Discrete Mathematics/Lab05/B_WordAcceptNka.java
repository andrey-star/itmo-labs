import java.io.*;
import java.util.HashSet;

public class B_WordAcceptNka {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("problem2.in")));
		String s = in.readLine();
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		HashSet<Integer> term = new HashSet<>();
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			term.add(Integer.parseInt(line[i]) - 1);
		}
		boolean[][][] mat = new boolean[n]['z' - 'a' + 1][n];
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			char c = line[2].charAt(0);
			mat[a][c - 'a'][b] = true;
		}
		HashSet<Integer> curStates = new HashSet<>();
		curStates.add(0);
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			HashSet<Integer> oldStates = new HashSet<>(curStates);
			curStates = new HashSet<>();
			for (int state : oldStates) {
				for (int j = 0; j < n; j++) {
					if (mat[state][c - 'a'][j]) {
						curStates.add(j);
					}
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("problem2.out"));
		boolean ok = false;
		for (int endState : curStates) {
			if (term.contains(endState)) {
				ok = true;
				break;
			}
		}
		out.println(ok ? "Accepts" : "Rejects");
		out.close();
	}
	
}
