import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class D_Birthdays {
	
	private static boolean dfs(int u, List<Integer>[] g, boolean[] used, int[] matching) {
		used[u] = true;
		for (int v : g[u]) {
			if (matching[v] == -1) {
				matching[v] = u;
				return true;
			}
			if (!used[matching[v]] && dfs(matching[v], g, used, matching)) {
				matching[v] = u;
				return true;
			}
		}
		return false;
	}
	
	private static void dfs2(int u, List<Integer>[] g, boolean[] usedLeft, boolean[] usedRight, int[] matching) {
		usedLeft[u] = true;
		for (int v : g[u]) {
			if (matching[v] == u || usedRight[v]) {
				continue;
			}
			usedRight[v] = true;
			int to = matching[v];
			if (to != -1 && !usedLeft[to]) {
				dfs2(to, g, usedLeft, usedRight, matching);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
//		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" ");
		int k = Integer.parseInt(line[0]);
		for (int i = 0; i < k; i++) {
			solve(in);
		}
	}
	
	private static void solve(BufferedReader in) throws IOException {
		String[] line = in.readLine().trim().split(" ");
		int left = Integer.parseInt(line[0]);
		int right = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[left];
		for (int i = 0; i < left; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < left; i++) {
			line = in.readLine().trim().split(" ");
			boolean[] edges = new boolean[right];
			for (int j = 0; j < line.length - 1; j++) {
				int a = Integer.parseInt(line[j]) - 1;
				edges[a] = true;
			}
			for (int j = 0; j < right; j++) {
				if (!edges[j]) {
					g[i].add(j);
				}
			}
		}
		boolean[] used = new boolean[left];
		int[] matching = new int[right];
		Arrays.fill(matching, -1);
		for (int i = 0; i < left; i++) {
			Arrays.fill(used, false);
			dfs(i, g, used, matching);
		}
		boolean[] inM = new boolean[left];
		for (int a : matching) {
			if (a != -1) {
				inM[a] = true;
			}
		}
		boolean[] usedLeft = new boolean[left];
		boolean[] usedRight = new boolean[right];
		for (int i = 0; i < left; i++) {
			if (!inM[i]) {
				dfs2(i, g, usedLeft, usedRight, matching);
			}
		}
		List<Integer> leftPlus = IntStream.range(0, left)
				.filter(i -> usedLeft[i])
				.boxed()
				.collect(Collectors.toList());
		List<Integer> rightMinus = IntStream.range(0, right)
				.filter(i -> !usedRight[i])
				.boxed()
				.collect(Collectors.toList());
		System.out.println(leftPlus.size() + rightMinus.size());
		System.out.println(leftPlus.size() + " " + rightMinus.size());
		for (Integer minus : leftPlus) {
			System.out.print(minus + 1 + " ");
		}
		System.out.println();
		for (Integer plus : rightMinus) {
			System.out.print(plus + 1 + " ");
		}
		System.out.println();
		System.out.println();
	}
	
}