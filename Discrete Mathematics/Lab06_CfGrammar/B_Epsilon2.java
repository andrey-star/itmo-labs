import java.io.*;
import java.util.*;


public class B_Epsilon2 {
	
	private static int nonTerminals(String s) {
		int res = 0;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (Character.isUpperCase(c)) {
				res++;
			}
		}
		return res;
	}
	
	private static boolean onlyLowerCase(String s) {
		if (s.length() == 0) {
			return false;
		}
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (!Character.isLowerCase(c)) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("epsilon.in")));
		String[] line = in.readLine().trim().split(" +");
		int rulesNumTotal = Integer.parseInt(line[0]);
		List<Pair> rules = new ArrayList<>();
		Set<Integer> epsilons = new TreeSet<>();
		for (int i = 0; i < rulesNumTotal; i++) {
			line = in.readLine().trim().split(" +");
			char left = line[0].charAt(0);
			String right = "";
			if (line.length == 3) {
				right = line[2];
				if (onlyLowerCase(right)) {
					continue;
				}
			} else {
				epsilons.add(left - 'A');
			}
			rules.add(new Pair(left, right));
		}
		
		while (true) {
			boolean changed = false;
			mark:for (Pair rule : rules) {
				int left = rule.left - 'A';
				String right = rule.right;
				if (epsilons.contains(left)) {
					continue;
				}
				for (int j = 0; j < right.length(); j++) {
					char c = right.charAt(j);
					if (Character.isLowerCase(c) || !epsilons.contains(c - 'A')) {
						continue mark;
					}
				}
				epsilons.add(left);
				changed = true;
				break;
			}
			if (!changed) {
				break;
			}
		}
		PrintWriter out = new PrintWriter(new File("epsilon.out"));
		for (int i : epsilons) {
			out.print(((char) (i + 'A')) + " ");
		}
		out.close();
	}
	
	static class Pair {
		char left;
		String right;
		
		Pair(char a, String b) {
			this.left = a;
			this.right = b;
		}
		
		@Override
		public String toString() {
			return "Pair{" +
					"left=" + left +
					", right='" + right + '\'' +
					'}';
		}
	}
	
	
}
