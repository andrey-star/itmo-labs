import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class F_Trees {
	
	private static final int MOD = (int) (1e9 + 7);
	private static final Map<Integer, Long> mem = new HashMap<>();
	
	private static long trees(int m, long[] trees, int[] c) {
		if (m < 0) {
			return 0;
		}
		if (trees[m] == -1) {
			trees[m] = 0;
			for (int rootWeight : c) {
				long add = mem.computeIfAbsent(m - rootWeight, sum -> {
					long res = 0;
					for (int leftWeight = 0; leftWeight <= sum; leftWeight++) {
						int rightWeight = sum - leftWeight;
						long l = trees(leftWeight, trees, c) * trees(rightWeight, trees, c);
						res += l;
						res %= MOD;
					}
					return res;
				});
				trees[m] += add;
				trees[m] %= MOD;
			}
		}
		return trees[m];
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int k = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int[] c = new int[k];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			c[i] = Integer.parseInt(line[i]);
		}
		in.close();
		long[] trees = new long[m + 1];
		Arrays.fill(trees, -1);
		trees[0] = 1;
		for (int i = 1; i <= m; i++) {
			trees(i, trees, c);
		}
		for (int i = 1; i < trees.length; i++) {
			long tree = trees[i];
			System.out.print(tree + " ");
		}
		System.out.println();
	}
	
}
