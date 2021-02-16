package forloop;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class ForLoopGV {
	
	private int index;
	
	public ForLoopGV() {
		this.index = -1;
	}
	
	public String getGraph(ForLoopParser.Node root) throws IOException {
		StringBuilder sb = new StringBuilder("digraph G {\n");
		traverse(root, -1, sb);
		sb.append("}\n");
		return sb.toString();
	}
	
	public void generateImage(String graph, Path dest, String fileName) throws IOException {
		try (BufferedWriter out = Files.newBufferedWriter(dest.resolve(fileName + ".dot"))) {
			out.write(graph);
			Runtime.getRuntime().exec("dot -Tpdf %1$s/%2$s.dot -o %1$s/%2$s.pdf".formatted(dest.toString(), fileName));
		}
	}
	
	private void traverse(ForLoopParser.Node t, int pIndex, StringBuilder sb) {
		index++;
		sb.append(String.format("\t%d [label = \"%s\"]\n", index, t.value));
		if (pIndex != -1) {
			sb.append(String.format("\t%d -> %d\n", pIndex, index));
		}
		int p = index;
		for (var ch : t.children) {
			traverse(ch, p, sb);
		}
	}
	
}