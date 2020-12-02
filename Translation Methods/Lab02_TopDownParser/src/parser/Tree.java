package parser;

import token.Token;

import java.util.Arrays;
import java.util.List;

public class Tree {
	public String node;
	
	public List<Tree> children;
	
	public Tree(String node, Tree... children) {
		this.node = node;
		this.children = Arrays.asList(children);
	}
	
	public Tree(String node) {
		this.node = node;
	}
	
	public Tree(Token var) {
		this.node = var.toString();
	}
	
	@Override
	public String toString() {
		return node + (children == null ? "" : children.toString());
	}
}
