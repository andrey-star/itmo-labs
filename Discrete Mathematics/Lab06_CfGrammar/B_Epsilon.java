import java.io.*;
import java.util.*;

public class B_Epsilon {
	
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
		for (int i = 0; i < rulesNumTotal; i++) {
			line = in.readLine().trim().split(" +");
			String left = line[0];
			String right = (line.length == 3 ? line[2] : "");
			if (onlyLowerCase(right)) {
				continue;
			}
			rules.add(new Pair(left.charAt(0), right));
		}
		int l = 'z' - 'a' + 1;
		boolean[] isEpsilon = new boolean[l];
		//noinspection unchecked
		Set<Integer>[] concernedRules = new HashSet[l];
		for (int i = 0; i < l; i++) {
			concernedRules[i] = new HashSet<>();
		}
		int[] counter = new int[rules.size()];
		List<Integer> q = new LinkedList<>();
		for (int i = 0; i < rules.size(); i++) {
			Pair rule = rules.get(i);
			for (int j = 0; j < rule.b.length(); j++) {
				char c = rule.b.charAt(j);
				if (Character.isUpperCase(c)) {
					concernedRules[c - 'A'].add(i);
					counter[i]++;
				}
			}
			if (counter[i] == 0) {
				isEpsilon[rule.a - 'A'] = true;
				q.add(rule.a - 'A');
			}
		}
		while (!q.isEmpty()) {
			System.out.println(q);
			int curNonTerm = q.remove(0);
			for (int rule : concernedRules[curNonTerm]) {
				counter[rule]--;
				if (counter[rule] == 0) {
					int left = rules.get(rule).a - 'A';
					isEpsilon[left] = true;
					q.add(left);
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("epsilon.out"));
		for (int i = 0; i < isEpsilon.length; i++) {
			if (isEpsilon[i]) {
				System.out.print(((char) (i + 'A')) + " ");
			}
		}
		out.close();
	}
	
	private static class Pair {
		char a;
		String b;
		
		Pair(char a, String b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public String toString() {
			return "Pair{" +
					"left=" + a +
					", right='" + b + '\'' +
					'}';
		}
	}
	
}
