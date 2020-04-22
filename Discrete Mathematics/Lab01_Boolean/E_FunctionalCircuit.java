import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;

public class E_FunctionalCircuit {
	
	static class Node {
		int[] inputs;
		int[] table;
		int result;
		final String type;
		int depth;
		
		Node() {
			type = "var";
			depth = 0;
		}
		
		Node(int n, int[] input, int[] table) {
			inputs = new int[n];
			System.arraycopy(input, 0, inputs, 0, input.length);
			this.table = new int[table.length];
			System.arraycopy(table, 0, this.table, 0, table.length);
			result = -1;
			type = "fun";
		}
		
		void setVar(int n) {
			result = n;
		}
		
		void run(int[] inputs) {
			int binaryForm = 0;
			for (int i = 0; i < inputs.length; i++) {
				binaryForm += (inputs[i] << (inputs.length - i - 1));
			}
			result = table[binaryForm];
		}
		
		@Override
		public String toString() {
			return "Node{" +
					"inputs=" + Arrays.toString(inputs) +
					", table=" + Arrays.toString(table) +
					", result=" + result +
					", type='" + type + '\'' +
					", depth=" + depth +
					'}';
		}
	}
	
	static class Circuit {
		
		final ArrayList<Node> circuit;
		
		Circuit() {
			circuit = new ArrayList<>();
		}
		
		void add(Node node) {
			if (node.inputs != null) {
				int maxDepth = 0;
				for (int i = 0; i < node.inputs.length; i++) {
					maxDepth = Math.max(maxDepth, circuit.get(node.inputs[i]).depth);
				}
				node.depth = maxDepth + 1;
			}
			circuit.add(node);
		}
		
		int run(int[] inputs) {
			int curInput = 0;
			for (Node cur : circuit) {
				if (cur.type.equals("var")) {
					cur.setVar(inputs[curInput++]);
				} else {
					int[] input = new int[cur.inputs.length];
					for (int j = 0; j < input.length; j++) {
						input[j] = circuit.get(cur.inputs[j]).result;
					}
					cur.run(input);
				}
			}
			return circuit.get(circuit.size() - 1).result;
		}
		
		@Override
		public String toString() {
			return "Circuit{" +
					"circuit=" + circuit +
					'}';
		}
		
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		Circuit circuit = new Circuit();
		int arg = 0;
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" ");
			int m = Integer.parseInt(line[0]);
			if (m == 0) {
				circuit.add(new Node());
				arg++;
			} else {
				int[] inputs = new int[m];
				for (int j = 0; j < m; j++) {
					inputs[j] = Integer.parseInt(line[j + 1]) - 1;
				}
				line = in.readLine().trim().split(" ");
				int[] table = new int[1 << m];
				for (int j = 0; j < (1 << m); j++) {
					table[j] = Integer.parseInt(line[j]);
				}
				circuit.add(new Node(m, inputs, table));
			}
		}
		int depth = circuit.circuit.get(circuit.circuit.size() - 1).depth;
		System.out.println(depth);
		for (int i = 0; i < (1 << arg); i++) {
			int[] inputs = new int[arg];
			for (int j = 0; j < arg; j++) {
				inputs[j] = (i >> (arg - j - 1)) & 1;
			}
			System.out.print(circuit.run(inputs));
		}
		System.out.println();
	}
	
}
