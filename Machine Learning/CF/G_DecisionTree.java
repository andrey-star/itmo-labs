import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class G_DecisionTree {
	
	int n, m, k, h;
	List<Entry> entries;
	BufferedReader in;
	
	public static void main(String[] args) throws IOException {
		new G_DecisionTree().run();
	}
	
	private void run() throws IOException {
		in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		m = Integer.parseInt(line[0]);
		k = Integer.parseInt(line[1]);
		h = Integer.parseInt(line[2]);
		readDataset();
		in.close();
		
		DecisionTree dt = new DecisionTree(entries, h);
		dt.train();
		System.out.println(dt.curId);
		System.out.println(dt);
	}
	
	private void readDataset() throws IOException {
		String[] line = in.readLine().trim().split(" +");
		n = Integer.parseInt(line[0]);
		entries = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			List<Integer> features = new ArrayList<>();
			line = in.readLine().trim().split(" +");
			for (int j = 0; j < m + 1; j++) {
				int a = Integer.parseInt(line[j]);
				if (j == m) {
					entries.add(new Entry(features, a - 1));
				} else {
					features.add(a);
				}
			}
		}
		
	}
	
	private static class Entry {
		List<Integer> x;
		int y;
		
		public Entry(List<Integer> x, int y) {
			this.x = x;
			this.y = y;
		}
		
		@Override
		public String toString() {
			return x + " -> " + y;
		}
	}
	
	private class DecisionTree {
		private final int maxDepth;
		Node root;
		int curId = 0;
		int[] freq; // used by getMostFreqClass
		
		public DecisionTree(List<Entry> s, int maxDepth) {
			this.maxDepth = maxDepth;
			freq = new int[k];
			Leaf baseNode = new Leaf(s);
			baseNode.c = getMostFreqClass(s);
			root = new Node(curId++, baseNode);
		}
		
		public void train() {
			train(root, 0);
		}
		
		private void train(Node node, int curDepth) {
			Leaf curLeaf = (Leaf) node.node;
			if (curDepth == maxDepth) {
				curLeaf.c = getMostFreqClass(curLeaf.s);
				return;
			}
			int minClass = 0;
			int maxClass = 0;
			for (Entry entry : curLeaf.s) {
				minClass = Math.min(minClass, entry.y);
				maxClass = Math.max(maxClass, entry.y);
			}
			if (minClass == maxClass) {
				curLeaf.c = minClass;
				return;
			}
			
			Splitter sp = n < 100 ? splitEntropy(curLeaf) : splitGini(curLeaf);
			if (sp == null) {
				curLeaf.c = getMostFreqClass(curLeaf.s);
				return;
			}
			curLeaf.s.sort(Comparator.comparing(e -> e.x.get(sp.f)));
			int splitIndex = -1;
			for (int i = 0; i < curLeaf.s.size(); i++) {
				if (curLeaf.s.get(i).x.get(sp.f) > sp.splitValue) {
					splitIndex = i;
					break;
				}
			}
			Node left = new Node(curId++, new Leaf(curLeaf.s.subList(0, splitIndex)));
			Node right = new Node(curId++, new Leaf(curLeaf.s.subList(splitIndex, curLeaf.s.size())));
			train(left, curDepth + 1);
			train(right, curDepth + 1);
			node.node = new InnerNode(left, right, sp.f, sp.splitValue);
		}
		
		private Splitter splitGini(Leaf l) {
			List<Entry> s = l.s;
			double maxScore = 0;
			int bestF = -1;
			double bestSplitValue = -1;
			for (int f = 0; f < m; f++) {
				int finalI = f;
				s.sort(Comparator.comparingInt(e -> e.x.get(finalI)));
				if (s.get(0).x.get(f).equals(s.get(s.size() - 1).x.get(f))) {
					continue; // same value for every object
				}
				int left = 0;
				int right = 0;
				double[] cl = new double[k];
				double[] cr = new double[k];
				for (Entry e : s) {
					right += 2 * cr[e.y] + 1;
					cr[e.y]++;
				}
				for (int split = 0; split < s.size(); split++) {
					int curC = s.get(split).y;
					if (split != 0 && !s.get(split - 1).x.get(f).equals(s.get(split).x.get(f))) {
						double score = 1.0 * left / split + 1.0 * right / (s.size() - split);
						if (score > maxScore) {
							maxScore = score;
							bestF = f;
							bestSplitValue = (s.get(split - 1).x.get(f) + s.get(split).x.get(f)) / 2.0;
						}
					}
					left += 2 * cl[curC] + 1;
					cl[curC]++;
					
					right += -2 * cr[curC] + 1;
					cr[curC]--;
				}
			}
			if (bestF == -1) {
				return null;
			}
			return new Splitter(bestF, bestSplitValue);
		}
		
		private Splitter splitEntropy(Leaf l) {
			List<Entry> s = l.s;
			double minScore = Integer.MAX_VALUE;
			int bestF = -1;
			double bestSplitValue = -1;
			for (int f = 0; f < m; f++) {
				int finalI = f;
				s.sort(Comparator.comparingInt(e -> e.x.get(finalI)));
				if (s.get(0).x.get(f).equals(s.get(s.size() - 1).x.get(f))) {
					continue; // same value for every object
				}
				double[] cl = new double[k];
				double[] cr = new double[k];
				for (Entry e : s) {
					cr[e.y]++;
				}
				for (int split = 0; split < s.size(); split++) {
					int curC = s.get(split).y;
					if (split != 0 && !s.get(split - 1).x.get(f).equals(s.get(split).x.get(f))) {
						double score = entropy(cl, split) * split + entropy(cr, s.size() - split) * (s.size() - split);
						if (score < minScore) {
							minScore = score;
							bestF = f;
							bestSplitValue = (s.get(split - 1).x.get(f) + s.get(split).x.get(f)) / 2.0;
						}
					}
					cl[curC]++;
					cr[curC]--;
				}
			}
			if (bestF == -1) {
				return null;
			}
			return new Splitter(bestF, bestSplitValue);
		}
		
		private double entropy(double[] a, int len) {
			double sum = 0;
			for (double v : a) {
				if (v != 0) {
					sum -= v / len * Math.log(v / len);
				}
			}
			return sum;
		}
		
		int getMostFreqClass(List<Entry> s) {
			Arrays.fill(freq, 0);
			for (Entry entry : s) {
				freq[entry.y]++;
			}
			int max = 0;
			int maxClass = -1;
			for (int i = 0; i < freq.length; i++) {
				if (freq[i] > max) {
					max = freq[i];
					maxClass = i;
				}
			}
			return maxClass;
		}
		
		@Override
		public String toString() {
			Map<Integer, String> nodeStrings = new TreeMap<>();
			fillNodes(root, nodeStrings);
			StringBuilder sb = new StringBuilder();
			for (String value : nodeStrings.values()) {
				sb.append(value).append("\n");
			}
			return sb.toString();
		}
		
		private void fillNodes(Node node, Map<Integer, String> res) {
			res.put(node.id, node.node.toString());
			if (node.node instanceof InnerNode) {
				InnerNode in = (InnerNode) node.node;
				fillNodes(in.left, res);
				fillNodes(in.right, res);
			}
		}
		
		private class Splitter {
			int f;
			double splitValue;
			
			public Splitter(int f, double splitValue) {
				this.f = f;
				this.splitValue = splitValue;
			}
		}
		
		private class Node {
			int id;
			BaseNode node;
			
			public Node(int id, BaseNode node) {
				this.id = id;
				this.node = node;
			}
			
		}
		
		private class BaseNode {
		}
		
		private class InnerNode extends BaseNode {
			Node left, right;
			int f;
			double b;
			
			public InnerNode(Node left, Node right, int f, double b) {
				this.left = left;
				this.right = right;
				this.f = f;
				this.b = b;
			}
			
			@Override
			public String toString() {
				return String.format(Locale.US, "Q %d %.15f %d %d", f + 1, b, left.id + 1, right.id + 1);
			}
		}
		
		private class Leaf extends BaseNode {
			List<Entry> s;
			int c = -1;
			
			public Leaf(List<Entry> s) {
				this.s = s;
			}
			
			@Override
			public String toString() {
				return "C " + (c + 1);
			}
		}
	}
	
	
}
