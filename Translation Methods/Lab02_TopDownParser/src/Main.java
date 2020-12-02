import parser.Parser;
import parser.Tree;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

public class Main {
	static int index = 0;
	static Map<String, Integer> a = new HashMap<>();
	
	public static void main(String[] args) throws ParseException {
		Parser parser = new Parser();
		Tree tree = parser.parse("for (var xxx = 100; xxx >= -100; xxx--)");
		System.out.println(visualize(tree));
	}
	
	private static String visualize(Tree t) {
		StringBuilder sb = new StringBuilder("0 [label = \"S\"]");
		dfs(t, null, sb);
		return sb.toString();
	}
	
	private static void dfs(Tree t, String p, StringBuilder sb) {
		if (t.children != null) {
			if (!a.containsKey(t.node)) {
				a.put(t.node, index);
				index++;
			}
			if (p != null) {
				sb.append(String.format("%d [label = \"%s\"]\n", a.get(t.node), t.node));
				sb.append(String.format("%d -> %d\n", a.get(p), a.get(t.node)));
			}
			for (Tree ch : t.children) {
				dfs(ch, t.node, sb);
			}
		} else {
			sb.append(String.format("%d [label = \"%s\"]\n", index, t.node));
			sb.append(String.format("%s -> %d\n", a.get(p), index));
			index++;
		}
	}
	
}
