import java.io.*;
import java.util.*;

public class A_Automaton {
	
	private static boolean check(String s, boolean[][][] g, int start) {
		int n = g.length;
		HashSet<Integer> curStates = new HashSet<>();
		curStates.add(start);
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			HashSet<Integer> oldStates = new HashSet<>(curStates);
			curStates = new HashSet<>();
			for (int state : oldStates) {
				for (int j = 0; j < n; j++) {
					if (g[state][c - 'a'][j]) {
						curStates.add(j);
					}
				}
			}
		}
		boolean ok = false;
		for (int endState : curStates) {
			if (endState == n - 1) {
				ok = true;
				break;
			}
		}
		return ok;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("automaton.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		char start = line[1].charAt(0);
		int l = 'z' - 'a' + 1;
		boolean[][][] g = new boolean[l + 1][l][l + 1]; // l - terminal
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			char state = line[0].charAt(0);
			String s = line[2];
			if (s.length() == 1) {
				char c = s.charAt(0);
				g[state - 'A'][c - 'a'][l] = true;
			} else {
				char c = s.charAt(0);
				char nextState = s.charAt(1);
				g[state - 'A'][c - 'a'][nextState - 'A'] = true;
			}
		}
//		for (int i = 0; i < g.length; i++) {
//			for (int j = 0; j < g[0].length; j++) {
//				for (int k = 0; k < g[0][0].length; k++) {
//					if (g[i][j][k]) {
//						System.out.println((char) (i + 'A') + " -> " + (char) (k + 'A') + " " + (char) (j + 'left'));
//					}
//				}
//			}
//		}
		PrintWriter out = new PrintWriter(new File("automaton.out"));
		int m = Integer.parseInt(in.readLine());
		for (int i = 0; i < m; i++) {
			String s = in.readLine();
			out.println(check(s, g, start - 'A') ? "yes" : "no");
		}
		out.close();
	}
	
}
