import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Ba3j extends AbstractTask {
	
	private Map<ReadPair, Integer> indices;
	private Map<Integer, ReadPair> indicesRev;
	
	public Ba3j() {
		super("ba3j");
	}
	
	public static void main(String[] args) throws IOException {
		new Ba3j().test();
	}
	
	public void test(Path test) throws FileNotFoundException {
		System.out.println(test.toFile());
		Scanner in = new Scanner(new InputStreamReader(new FileInputStream(test.toFile())));
		int k = in.nextInt();
		int d = in.nextInt();
		List<ReadPair> pairedReads = new ArrayList<>();
		while (in.hasNext()) {
			pairedReads.add(ReadPair.fromPairedRead(in.next()));
		}
		System.out.println(solve(k, d, pairedReads));
	}
	
	private String solve(int k, int d, List<ReadPair> pairedReads) {
		setIndices(pairedReads);
		int n = indices.size();
		@SuppressWarnings("unchecked")
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		Set<Integer> ba = IntStream.range(0, n).boxed().collect(Collectors.toSet());
		for (ReadPair read : pairedReads) {
			int u = indices.get(read.getLeftPair());
			int v = indices.get(read.getRightPair());
			ba.remove(v);
			g[u].add(v);
		}
		List<Integer> euler = euler(g, ba.stream().findAny().get());
		StringBuilder res = new StringBuilder();
		for (int i = 0; i < euler.size(); i++) {
			int u = euler.get(i);
			if (i < euler.size() - 1) {
				res.append(indicesRev.get(u).first.charAt(0));
			} else {
				res.append(indicesRev.get(u).first);
				for (int j = euler.size() - d - k; j < euler.size(); j++) {
					u = euler.get(j);
					String second = indicesRev.get(u).second;
					res.append(second.charAt(second.length() - 1));
				}
				break;
				
			}
		}
		return res.toString();
	}
	
	private void printGraph(List<Integer>[] g) {
		for (int i = 0; i < g.length; i++) {
			List<Integer> u = g[i];
			for (int v : u) {
				System.out.printf("%s -> %s%n", indicesRev.get(i), indicesRev.get(v));
			}
			System.out.println();
		}
	}
	
	private Map<ReadPair, Integer> setIndices(List<ReadPair> pairedReads) {
		indices = new HashMap<>();
		indicesRev = new HashMap<>();
		int curIndex = 0;
		for (ReadPair read : pairedReads) {
			ReadPair left = read.getLeftPair();
			ReadPair right = read.getRightPair();
			if (indices.get(left) == null) {
				indices.put(left, curIndex);
				indicesRev.put(curIndex, left);
				curIndex++;
			}
			if (indices.get(right) == null) {
				indices.put(right, curIndex);
				indicesRev.put(curIndex, right);
				curIndex++;
			}
		}
		return indices;
	}
	
	private List<Integer> euler(List<Integer>[] g, int s) {
		int[] curEdge = new int[g.length];
		List<Integer> res = new ArrayList<>();
		ArrayDeque<Integer> stack = new ArrayDeque<>();
		stack.push(s);
		while (!stack.isEmpty()) {
			int u = stack.pop();
			while (curEdge[u] < g[u].size()) {
				stack.push(u);
				u = g[u].get(curEdge[u]++);
			}
			res.add(u);
		}
		Collections.reverse(res);
		return res;
	}
	
	private static class ReadPair {
		
		int k;
		String first;
		String second;
		
		public ReadPair(String first, String second) {
			this.first = first;
			this.second = second;
			this.k = first.length();
		}
		
		public static ReadPair fromPairedRead(String concat) {
			String[] split = concat.split("\\|");
			return new ReadPair(split[0], split[1]);
		}
		
		private ReadPair getLeftPair() {
			String firstLeft = first.substring(0, k - 1);
			String secondLeft = second.substring(0, k - 1);
			return new ReadPair(firstLeft, secondLeft);
		}
		
		private ReadPair getRightPair() {
			String firstRight = first.substring(1, k);
			String secondRight = second.substring(1, k);
			return new ReadPair(firstRight, secondRight);
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof ReadPair)) return false;
			ReadPair pair = (ReadPair) o;
			return Objects.equals(first, pair.first) && Objects.equals(second, pair.second);
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(first, second);
		}
		
		@Override
		public String toString() {
			return String.format("(%s, %s)", first, second);
		}
	}
	
	private static class Edge {
		int u;
		int v;
		boolean used;
		
		Edge(int u, int v) {
			this.u = u;
			this.v = v;
			this.used = false;
		}
	}
}
