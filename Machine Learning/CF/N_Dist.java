import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class N_Dist {
	
	public static void main(String[] args) throws IOException {
		new N_Dist().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int k = Integer.parseInt(in.readLine());
		int n = Integer.parseInt(in.readLine());
		int[] x = new int[n];
		int[] y = new int[n];
		List<List<Integer>> entries1 = new ArrayList<>();
		for (int i = 0; i < k; i++) {
			entries1.add(new ArrayList<>());
		}
		List<Pair> entries2 = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			x[i] = Integer.parseInt(line[0]);
			y[i] = Integer.parseInt(line[1]) - 1;
			entries1.get(y[i]).add(x[i]);
			entries2.add(new Pair(x[i], y[i]));
		}
		System.out.println(inClassDist(entries1));
		System.out.println(outClassDist(entries2, k));
	}
	
	private long inClassDist(List<List<Integer>> entries) {
		for (List<Integer> integers : entries) {
			Collections.sort(integers);
		}
		long res = 0;
		for (List<Integer> xs : entries) {
			long prefSum = 0;
			long sufSum = xs.stream().reduce(0, Integer::sum);
			for (int i = 0, xsSize = xs.size(); i < xsSize; i++) {
				int x = xs.get(i);
				prefSum += x;
				sufSum -= x;
				res += ((long) x * (i + 1) - prefSum) - ((long) x * (xsSize - i - 1) - sufSum);
			}
		}
		return res;
	}
	
	private long outClassDist(List<Pair> entries, int k) {
		entries.sort(Comparator.comparing(p -> p.x));
		long[] prefSum = new long[k];
		long[] prefCount = new long[k];
		long prefTotal = 0;
		long[] sufSum = new long[k];
		long[] sufCount = new long[k];
		long sufTotal = 0;
		for (Pair entry : entries) {
			sufSum[entry.y] += entry.x;
			sufCount[entry.y]++;
			sufTotal += entry.x;
		}

		long res = 0;
		for (int i = 0; i < entries.size(); i++) {
			Pair pair = entries.get(i);
			prefSum[pair.y] += pair.x;
			prefCount[pair.y]++;
			prefTotal += pair.x;
			
			sufSum[pair.y] -= pair.x;
			sufCount[pair.y]--;
			sufTotal -= pair.x;
			
			res += ((long) pair.x * (i + 1 - prefCount[pair.y]) - (prefTotal - prefSum[pair.y])) -
					((long) pair.x * (entries.size() - i - 1 - sufCount[pair.y]) - (sufTotal - sufSum[pair.y]));
		}
		return res;
	}
	
	private static class Pair {
		int x;
		int y;
		
		public Pair(int x, int y) {
			this.x = x;
			this.y = y;
		}
	}
	
}
