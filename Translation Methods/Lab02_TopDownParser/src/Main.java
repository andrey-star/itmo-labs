import parser.Parser;
import parser.Node;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.ParseException;

public class Main {
	static int index = -1;
	
	public static void main(String[] args) throws ParseException, IOException {
		Parser parser = new Parser();
		Node tree = parser.parse("for (var xxx = 100; xxx >= -100; xxx--)");
		visualize(tree, "graph");
	}
	
	private static void visualize(Node root, String fileName) throws IOException {
		StringBuilder sb = new StringBuilder("digraph G {\n");
		dfs(root, -1, sb);
		sb.append("}\n");
		try (PrintWriter out = new PrintWriter(fileName + ".dot")) {
			out.println(sb.toString());
			Runtime.getRuntime().exec("dot -Tpng " + fileName + ".dot -o " + fileName + ".png");
		}
	}
	
	private static void dfs(Node t, int pIndex, StringBuilder sb) {
		index++;
		sb.append(String.format("%d [label = \"%s\"]\n", index, t.node));
		if (pIndex != -1) {
			sb.append(String.format("%d -> %d\n", pIndex, index));
		}
		int p = index;
		for (Node ch : t.children) {
			dfs(ch, p, sb);
		}
	}
	
}
