import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

public class I_MatrixFunction {
	
	public static void main(String[] args) throws IOException {
		new I_MatrixFunction().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		List<Node> nodes = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			String t = line[0];
			if (t.equals("var")) {
				int r = Integer.parseInt(line[1]);
				int c = Integer.parseInt(line[2]);
				nodes.add(new Var(r, c));
			} else if (t.equals("tnh")) {
				int x = Integer.parseInt(line[1]) - 1;
				nodes.add(new TanH(nodes.get(x)));
			} else if (t.equals("rlu")) {
				int aInv = Integer.parseInt(line[1]);
				int x = Integer.parseInt(line[2]) - 1;
				nodes.add(new ReLU(nodes.get(x), aInv));
			} else if (t.equals("mul")) {
				int a = Integer.parseInt(line[1]) - 1;
				int b = Integer.parseInt(line[2]) - 1;
				nodes.add(new Mul(nodes.get(a), nodes.get(b)));
			} else if (t.equals("sum") || t.equals("had")) {
				int len = Integer.parseInt(line[1]);
				List<Node> a = new ArrayList<>();
				for (int j = 0; j < len; j++) {
					int ind = Integer.parseInt(line[2 + j]) - 1;
					a.add(nodes.get(ind));
				}
				nodes.add(t.equals("sum") ? new Sum(a) : new HMul(a));
			} else {
				throw new AssertionError("Invalid command");
			}
		}
		for (int v = 0; v < m; v++) {
			Matrix values = nodes.get(v).values;
			for (int i = 0; i < values.w; i++) {
				line = in.readLine().trim().split(" +");
				for (int j = 0; j < values.h; j++) {
					values.set(i, j, Integer.parseInt(line[j]));
				}
			}
		}
		for (int v = 0; v < k; v++) {
			Matrix diff = nodes.get(n - k + v).diff;
			for (int i = 0; i < diff.w; i++) {
				line = in.readLine().trim().split(" +");
				for (int j = 0; j < diff.h; j++) {
					diff.set(i, j, Integer.parseInt(line[j]));
				}
			}
		}
		for (Node node : nodes) {
			node.updateValues();
		}
		for (int d = n - 1; d >= 0; d--) {
			nodes.get(d).feedDiff();
		}
		for (int v = 0; v < k; v++) {
			Matrix values = nodes.get(n - k + v).values;
			System.out.println(values);
		}
		for (int d = 0; d < m; d++) {
			Matrix diff = nodes.get(d).diff;
			System.out.println(diff);
		}
	}
	
	private static class Matrix {
		int w;
		int h;
		List<List<Double>> values;
		
		public Matrix(int w, int h, double init) {
			this.w = w;
			this.h = h;
			if (w == 0 || h == 0) {
				throw new AssertionError("Dimensions must be positive");
			}
			values = new ArrayList<>();
			for (int i = 0; i < w; i++) {
				List<Double> row = new ArrayList<>();
				for (int j = 0; j < h; j++) {
					row.add(init);
				}
				values.add(row);
			}
		}
		
		public Matrix(int w, int h) {
			this(w, h, 0.0);
		}
		
		public double get(int i, int j) {
			return values.get(i).get(j);
		}
		
		public void set(int i, int j, double val) {
			values.get(i).set(j, val);
		}
		
		public void add(int i, int j, double delta) {
			double cur = values.get(i).get(j);
			values.get(i).set(j, cur + delta);
		}
		
		public void mul(int i, int j, double factor) {
			double cur = values.get(i).get(j);
			values.get(i).set(j, cur * factor);
		}
		
		@Override
		public String toString() {
			return values.stream()
					.map(value -> value.stream()
							.map(d -> String.format(Locale.US, "%.10f", d))
							.collect(Collectors.joining(" ")))
					.collect(Collectors.joining("\n"));
		}
	}
	
	private abstract static class Node {
		Matrix values;
		Matrix diff;
		
		public Node(int w, int h, double init) {
			values = new Matrix(w, h, init);
			diff = new Matrix(w, h);
		}
		
		public Node(int w, int h) {
			this(w, h, 0.0);
		}
		
		
		abstract void updateValues();
		
		abstract void feedDiff();
	}
	
	private static class Var extends Node {
		public Var(int w, int h) {
			super(w, h);
		}
		
		@Override
		void updateValues() {
		}
		
		@Override
		void feedDiff() {
		}
	}
	
	private static class TanH extends Node {
		Node arg;
		
		public TanH(Node arg) {
			super(arg.values.w, arg.values.h);
			this.arg = arg;
		}
		
		@Override
		public void updateValues() {
			for (int i = 0; i < values.w; i++) {
				for (int j = 0; j < values.h; j++) {
					values.set(i, j, Math.tanh(arg.values.get(i, j)));
				}
			}
		}
		
		@Override
		public void feedDiff() {
			for (int i = 0; i < arg.diff.w; i++) {
				for (int j = 0; j < arg.diff.h; j++) {
					arg.diff.add(i, j, (1 - values.get(i, j) * values.get(i, j)) * diff.get(i, j));
				}
			}
		}
	}
	
	private static class ReLU extends Node {
		Node arg;
		double alphaInv;
		
		public ReLU(Node arg, double alphaInv) {
			super(arg.values.w, arg.values.h);
			this.arg = arg;
			this.alphaInv = alphaInv;
		}
		
		@Override
		public void updateValues() {
			for (int i = 0; i < values.w; i++) {
				for (int j = 0; j < values.h; j++) {
					double v = arg.values.get(i, j);
					if (v < 0) {
						values.set(i, j, v / alphaInv);
					} else {
						values.set(i, j, v);
					}
				}
			}
		}
		
		@Override
		public void feedDiff() {
			for (int i = 0; i < arg.diff.w; i++) {
				for (int j = 0; j < arg.diff.h; j++) {
					double v = arg.values.get(i, j);
					if (v < 0) {
						arg.diff.add(i, j, diff.get(i, j) / alphaInv);
					} else {
						arg.diff.add(i, j, diff.get(i, j));
					}
				}
			}
		}
	}
	
	private static class Mul extends Node {
		Node a, b;
		
		public Mul(Node a, Node b) {
			super(a.values.w, b.values.h);
			if (a.values.h != b.values.w) {
				throw new AssertionError("Invalid dimensions for matrix mul");
			}
			this.a = a;
			this.b = b;
		}
		
		@Override
		public void updateValues() {
			for (int i = 0; i < a.values.w; i++) {
				for (int j = 0; j < a.values.h; j++) {
					for (int k = 0; k < b.values.h; k++) {
						double v = a.values.get(i, j) * b.values.get(j, k);
						values.add(i, k, v);
					}
				}
			}
		}
		
		@Override
		public void feedDiff() {
			for (int i = 0; i < a.diff.w; i++) {
				for (int j = 0; j < a.diff.h; j++) {
					for (int k = 0; k < b.values.h; k++) {
						double dv = b.values.get(j, k) * diff.get(i, k);
						a.diff.add(i, j, dv);
					}
				}
			}
			
			for (int j = 0; j < a.diff.h; j++) {
				for (int k = 0; k < b.values.h; k++) {
					for (int i = 0; i < a.diff.w; i++) {
						double dv = a.values.get(i, j) * diff.get(i, k);
						b.diff.add(j, k, dv);
					}
				}
			}
		}
	}
	
	private static class Sum extends Node {
		List<Node> args;
		
		public Sum(List<Node> args) {
			super(args.get(0).values.w, args.get(0).values.h);
			for (Node arg : args) {
				if (arg.values.w != values.w || arg.values.h != values.h) {
					throw new AssertionError("Invalid dimensions for matrix sum");
				}
			}
			this.args = args;
		}
		
		@Override
		public void updateValues() {
			for (Node arg : args) {
				for (int i = 0; i < values.w; i++) {
					for (int j = 0; j < values.h; j++) {
						values.add(i, j, arg.values.get(i, j));
					}
				}
			}
		}
		
		@Override
		public void feedDiff() {
			for (Node arg : args) {
				for (int i = 0; i < arg.diff.w; i++) {
					for (int j = 0; j < arg.diff.h; j++) {
						arg.diff.add(i, j, diff.get(i, j));
					}
				}
			}
		}
	}
	
	private static class HMul extends Node {
		List<Node> args;
		
		public HMul(List<Node> args) {
			super(args.get(0).values.w, args.get(0).values.h, 1);
			for (Node arg : args) {
				if (arg.values.w != values.w || arg.values.h != values.h) {
					throw new AssertionError("Invalid dimensions for matrix Hadamard's mul");
				}
			}
			this.args = args;
		}
		
		@Override
		public void updateValues() {
			for (Node arg : args) {
				for (int i = 0; i < arg.values.w; i++) {
					for (int j = 0; j < arg.values.h; j++) {
						values.mul(i, j, arg.values.get(i, j));
					}
				}
			}
		}
		
		@Override
		public void feedDiff() {
			for (int n1 = 0; n1 < args.size(); n1++) {
				Node arg = args.get(n1);
				for (int i = 0; i < arg.diff.w; i++) {
					for (int j = 0; j < arg.diff.h; j++) {
						double factor = 1;
						for (int n2 = 0; n2 < args.size(); n2++) {
							Node arg2 = args.get(n2);
							if (n1 != n2) {
								factor *= arg2.values.get(i, j);
							}
						}
						arg.diff.add(i, j, factor * diff.get(i, j));
					}
				}
			}
		}
	}
}
