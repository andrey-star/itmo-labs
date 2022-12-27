package parser;

import token.Token;

import java.util.Arrays;
import java.util.List;

public class Node {
	public String node;
	
	public List<Node> children;
	
	public Node(String node, Node... nodes) {
		this.node = node;
		this.children = Arrays.asList(nodes);
	}
	
	public Node(Token var) {
		this(var.toString());
	}
	
	@Override
	public String toString() {
		return node + (children.isEmpty() ? "" : children.toString());
	}
}
