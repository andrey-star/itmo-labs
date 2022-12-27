import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.*;

public class Ba5j extends AbstractTask {
	
	public Ba5j() {
		super("ba5j");
	}
	
	public static void main(String[] args) throws IOException {
		new Ba5j().test();
	}
	
	public void test(Path test) throws FileNotFoundException {
		System.out.println(test.toFile());
		Scanner in = new Scanner(new InputStreamReader(new FileInputStream(test.toFile())));
		String s1 = in.next();
		String s2 = in.next();
		System.out.println(solve(s1, s2));
	}
	
	private String solve(String s1, String s2) {
		Result res = affineGapPenalty(s1, s2, new Blosum62(), 1, 11);
		return res.score + "\n" + res.s1 + "\n" + res.s2 + "\n";
	}
	
	private Result affineGapPenalty(String s1, String s2, ScoringMatrix s, int se, int so) {
		int n = s1.length();
		int m = s2.length();
		int[][] in = new int[n + 1][m + 1];
		int[][] de = new int[n + 1][m + 1];
		int[][] ma = new int[n + 1][m + 1];
		for (int i = 1; i <= n; i++) {
			for (int j = 1; j <= m; j++) {
				in[i][j] = Math.max(in[i - 1][j] - se, ma[i - 1][j] - so);
				de[i][j] = Math.max(de[i][j - 1] - se, ma[i][j - 1] - so);
				ma[i][j] = Math.max(Math.max(de[i][j], in[i][j]), ma[i - 1][j - 1] + s.score(s1.charAt(i - 1), s2.charAt(j - 1)));
			}
		}
		StringBuilder s1mod = new StringBuilder();
		StringBuilder s2mod = new StringBuilder();
		int i = n;
		int j = m;
		char maxArr = 'M';
		while (i != 0 && j != 0) {
			if (maxArr == 'I') {
				s1mod.append(s1.charAt(i - 1));
				if (in[i - 1][j] >= ma[i - 1][j] - se - so) {
					s2mod.append('-');
					maxArr = 'I';
				} else {
					s2mod.append('-');
					maxArr = 'M';
				}
				i--;
			} else if (maxArr == 'D') {
				s2mod.append(s2.charAt(j - 1));
				if (de[i][j - 1] > ma[i][j - 1] - se - so) {
					s1mod.append('-');
					maxArr = 'D';
				} else {
					s1mod.append('-');
					maxArr = 'M';
				}
				j--;
			} else if (maxArr == 'M') {
				int max = Math.max(Math.max(de[i][j], in[i][j]), ma[i - 1][j - 1] + s.score(s1.charAt(i - 1), s2.charAt(j - 1)));
				if (max == de[i][j]) {
					maxArr = 'D';
				} else if (max == in[i][j]) {
					maxArr = 'I';
				} else {
					s1mod.append(s1.charAt(i - 1));
					s2mod.append(s2.charAt(j - 1));
					i--;
					j--;
					maxArr = 'M';
				}
			}
		}
		return new Result(ma[n][m], s1mod.reverse().toString(), s2mod.reverse().toString());
	}
	
	private interface ScoringMatrix {
		
		int score(char a, char b);
		
	}
	
	private static class LectureScore implements ScoringMatrix {
		
		@Override
		public int score(char a, char b) {
			return a == b ? 2 : -2;
		}
	}
	
	private static class Blosum62 implements ScoringMatrix {
		
		private final List<Character> chars = List.of('A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'Y');
		private final String table = "  A C D E F G H I K L M N P Q R S T V W Y\n" +
		                             "A 4 0 -2 -1 -2 0 -2 -1 -1 -1 -1 -2 -1 -1 -1 1 0 0 -3 -2\n" +
		                             "C 0 9 -3 -4 -2 -3 -3 -1 -3 -1 -1 -3 -3 -3 -3 -1 -1 -1 -2 -2\n" +
		                             "D -2 -3 6 2 -3 -1 -1 -3 -1 -4 -3 1 -1 0 -2 0 -1 -3 -4 -3\n" +
		                             "E -1 -4 2 5 -3 -2 0 -3 1 -3 -2 0 -1 2 0 0 -1 -2 -3 -2\n" +
		                             "F -2 -2 -3 -3 6 -3 -1 0 -3 0 0 -3 -4 -3 -3 -2 -2 -1 1 3\n" +
		                             "G 0 -3 -1 -2 -3 6 -2 -4 -2 -4 -3 0 -2 -2 -2 0 -2 -3 -2 -3\n" +
		                             "H -2 -3 -1 0 -1 -2 8 -3 -1 -3 -2 1 -2 0 0 -1 -2 -3 -2 2\n" +
		                             "I -1 -1 -3 -3 0 -4 -3 4 -3 2 1 -3 -3 -3 -3 -2 -1 3 -3 -1\n" +
		                             "K -1 -3 -1 1 -3 -2 -1 -3 5 -2 -1 0 -1 1 2 0 -1 -2 -3 -2\n" +
		                             "L -1 -1 -4 -3 0 -4 -3 2 -2 4 2 -3 -3 -2 -2 -2 -1 1 -2 -1\n" +
		                             "M -1 -1 -3 -2 0 -3 -2 1 -1 2 5 -2 -2 0 -1 -1 -1 1 -1 -1\n" +
		                             "N -2 -3 1 0 -3 0 1 -3 0 -3 -2 6 -2 0 0 1 0 -3 -4 -2\n" +
		                             "P -1 -3 -1 -1 -4 -2 -2 -3 -1 -3 -2 -2 7 -1 -2 -1 -1 -2 -4 -3\n" +
		                             "Q -1 -3 0 2 -3 -2 0 -3 1 -2 0 0 -1 5 1 0 -1 -2 -2 -1\n" +
		                             "R -1 -3 -2 0 -3 -2 0 -3 2 -2 -1 0 -2 1 5 -1 -1 -3 -3 -2\n" +
		                             "S 1 -1 0 0 -2 0 -1 -2 0 -2 -1 1 -1 0 -1 4 1 -2 -3 -2\n" +
		                             "T 0 -1 -1 -1 -2 -2 -2 -1 -1 -1 -1 0 -1 -1 -1 1 5 0 -2 -2\n" +
		                             "V 0 -1 -3 -2 -1 -3 -3 3 -2 1 1 -3 -2 -2 -3 -2 0 4 -3 -1\n" +
		                             "W -3 -2 -4 -3 1 -2 -2 -3 -3 -2 -1 -4 -4 -2 -3 -3 -2 -3 11 2\n" +
		                             "Y -2 -2 -3 -2 3 -3 2 -1 -2 -1 -1 -2 -3 -1 -2 -2 -2 -1 2 7";
		
		private final Map<Character, Map<Character, Integer>> scores;
		
		public Blosum62() {
			scores = parseTable();
		}
		
		private Map<Character, Map<Character, Integer>> parseTable() {
			Map<Character, Map<Character, Integer>> res = new HashMap<>();
			table.lines().skip(1).forEach(line -> {
				String[] split = line.split(" ");
				char c = split[0].charAt(0);
				res.put(c, new HashMap<>());
				for (int i = 1; i < split.length; i++) {
					res.get(c).put(chars.get(i - 1), Integer.parseInt(split[i]));
				}
			});
			return res;
		}
		
		@Override
		public int score(char a, char b) {
			return scores.get(a).get(b);
		}
	}
	
	private static class Result {
		int score;
		String s1;
		String s2;
		
		public Result(int score, String s1, String s2) {
			this.score = score;
			this.s1 = s1;
			this.s2 = s2;
		}
	}
	
}
