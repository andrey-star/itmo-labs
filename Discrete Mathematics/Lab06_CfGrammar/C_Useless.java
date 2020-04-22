import java.io.*;
import java.util.*;


public class C_Useless {
	
	private static boolean allTerminals(String s) {
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (!Character.isLowerCase(c)) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("useless.in")));
		String[] line = in.readLine().trim().split(" +");
		int rulesNumTotal = Integer.parseInt(line[0]);
		char start = line[1].charAt(0);
		List<Pair> rules = new LinkedList<>();
		Set<Integer> available = new TreeSet<>();
		available.add(start - 'A');
		Set<Integer> terminal = new HashSet<>();
		for (int i = 0; i < rulesNumTotal; i++) {
			line = in.readLine().trim().split(" +");
			char left = line[0].charAt(0);
			String right = "";
			if (line.length == 3) {
				right = line[2];
			}
			if (allTerminals(right)) {
				terminal.add(left - 'A');
			}
			rules.add(new Pair(left, right));
			available.add(left - 'A');
			for (int j = 0; j < right.length(); j++) {
				char c = right.charAt(j);
				if (Character.isUpperCase(c)) {
					available.add(c - 'A');
				}
			}
		}
		
		// terminal
		while (true) {
			boolean changed = false;
			mark:
			for (Pair rule : rules) {
				int left = rule.left - 'A';
				String right = rule.right;
				if (terminal.contains(left)) {
					continue;
				}
				for (int j = 0; j < right.length(); j++) {
					char c = right.charAt(j);
					if (!Character.isLowerCase(c) && !terminal.contains(c - 'A')) {
						continue mark;
					}
				}
				terminal.add(left);
				changed = true;
				break;
			}
			if (!changed) {
				break;
			}
		}
		
		Set<Integer> delete = new TreeSet<>(Comparator.comparingInt(a -> -a));
		mark: for (int i = 0; i < rules.size(); i++) {
			Pair rule = rules.get(i);
			if (!terminal.contains(rule.left - 'A')) {
				delete.add(i);
				continue;
			}
			for (int j = 0; j < rule.right.length(); j++) {
				char c = rule.right.charAt(j);
				if (Character.isUpperCase(c) && !terminal.contains(c - 'A')) {
					delete.add(i);
					continue mark;
				}
			}
		}
		for (int rule : delete) {
			rules.remove(rule);
		}
		
		// reachable
		Set<Integer> reachable = new HashSet<>();
		reachable.add(start - 'A');
		while (true) {
			boolean changed = false;
			for (Pair rule : rules) {
				int left = rule.left - 'A';
				String right = rule.right;
				if (!reachable.contains(left)) {
					continue;
				}
				for (int j = 0; j < right.length(); j++) {
					char c = right.charAt(j);
					if (Character.isUpperCase(c) && !reachable.contains(c - 'A')) {
						changed = true;
						reachable.add(c - 'A');
					}
				}
			}
			if (!changed) {
				break;
			}
		}
//		System.out.print("Available: ");
//		for (int i : available) {
//			System.out.print(((char) (i + 'A')) + " ");
//		}
//		System.out.println();
//		System.out.print("Terminal: ");
//		for (int i : terminal) {
//			System.out.print(((char) (i + 'A')) + " ");
//		}
//		System.out.println();
//		System.out.print("Reachable: ");
//		for (int i : reachable) {
//			System.out.print(((char) (i + 'A')) + " ");
//		}
//		System.out.println();
//
		delete = new TreeSet<>(Comparator.comparingInt(a -> -a));
		mark: for (int i = 0; i < rules.size(); i++) {
			Pair rule = rules.get(i);
			if (!reachable.contains(rule.left - 'A')) {
				delete.add(i);
				continue;
			}
			for (int j = 0; j < rule.right.length(); j++) {
				char c = rule.right.charAt(j);
				if (Character.isUpperCase(c) && !reachable.contains(c - 'A')) {
					delete.add(i);
					continue mark;
				}
			}
		}
		for (int rule : delete) {
			rules.remove(rule);
		}
		Set<Integer> res = new TreeSet<>();
		for (Pair rule : rules) {
			res.add(rule.left - 'A');
			for (int i = 0; i < rule.right.length(); i++) {
				char c = rule.right.charAt(i);
				if (Character.isUpperCase(c)) {
					res.add(c - 'A');
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("useless.out"));
		for (int i : available) {
			if (!res.contains(i)) {
				out.print(((char) (i + 'A')) + " ");
			}
		}
		out.close();
	}
	
	static class Pair {
		final char left;
		final String right;
		
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
