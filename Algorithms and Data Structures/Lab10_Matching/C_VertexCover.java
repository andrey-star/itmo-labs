import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class C_VertexCover {
	
	private static void dfs(int u, List<Integer>[] g, boolean[] usedLeft, boolean[] usedRight, int[] matching) {
		usedLeft[u] = true;
		for (int v : g[u]) {
			if (matching[v] == u || usedRight[v]) {
				continue;
			}
			usedRight[v] = true;
			int to = matching[v];
			if (to != -1 && !usedLeft[to]) {
				dfs(to, g, usedLeft, usedRight, matching);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
//		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int left = Integer.parseInt(line[0]);
		int right = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[left];
		for (int i = 0; i < left; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < left; i++) {
			line = in.readLine().trim().split(" +");
			int k = Integer.parseInt(line[0]);
			for (int j = 1; j <= k; j++) {
				int a = Integer.parseInt(line[j]) - 1;
				g[i].add(a);
			}
		}
		int[] matching = new int[right];
		boolean[] inM = new boolean[left];
		Arrays.fill(matching, -1);
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < left; i++) {
			int a = Integer.parseInt(line[i]) - 1;
			if (a != -1) {
				matching[a] = i;
				inM[i] = true;
			}
		}
		boolean[] usedLeft = new boolean[left];
		boolean[] usedRight = new boolean[right];
		for (int i = 0; i < left; i++) {
			if (!inM[i]) {
				dfs(i, g, usedLeft, usedRight, matching);
			}
		}
		List<Integer> leftMinus = IntStream.range(0, left)
				.filter(i -> !usedLeft[i])
				.boxed()
				.collect(Collectors.toList());
		List<Integer> rightPlus = IntStream.range(0, right)
				.filter(i -> usedRight[i])
				.boxed()
				.collect(Collectors.toList());
		System.out.println(leftMinus.size() + rightPlus.size());
		System.out.print(leftMinus.size() + " ");
		for (Integer minus : leftMinus) {
			System.out.print(minus + 1 + " ");
		}
		System.out.println();
		System.out.print(rightPlus.size() + " ");
		for (Integer plus : rightPlus) {
			System.out.print(plus + 1 + " ");
		}
	}
	
}