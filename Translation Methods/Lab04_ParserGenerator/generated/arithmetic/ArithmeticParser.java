package arithmetic;

import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;

import java.lang.Math.*;
import java.util.function.DoubleUnaryOperator;
import java.util.function.DoubleBinaryOperator;
import java.util.Map;


public class ArithmeticParser {
	
	
	private Map<String, DoubleUnaryOperator> unaryOps = Map.of(
		"abs", Math::abs,
		"sqrt", Math::sqrt,
		"log", Math::log
	);
	private Map<String, DoubleBinaryOperator> binaryOps = Map.of(
		"pow", Math::pow,
		"min", Math::min,
		"max", Math::max
	);
	
	ArithmeticLexer lexer;
	ArithmeticToken curToken;
	
	public ArithmeticParser(ArithmeticLexer lexer) {
		this.lexer = lexer;
		curToken = this.lexer.next();
	}
	
	public class E {
		public Node _node = new Node("E");
		public Integer val;
		public String text() {
			return _node.toString();
		}
	}
	
	public E E() {
		var E0 = new E();
		
		switch (curToken.type()) {
			case IDENT, NUM, LPAREN, MINUS -> {
				var T1 = new T();
				var EP2 = new EP();
				
				var _T1 = T();
				T1.val = _T1.val;
				T1._node = _T1._node;
				{EP2.acc = T1.val;}
				E0._node.children.add(T1._node);
				
				var _EP2 = EP(EP2.acc);
				EP2.val = _EP2.val;
				EP2._node = _EP2._node;
				{E0.val = EP2.val;}
				E0._node.children.add(EP2._node);
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return E0;
	}
	
	public class EP {
		public Node _node = new Node("EP");
		public Integer acc;
		public Integer val;
		public String text() {
			return _node.toString();
		}
	}
	
	public EP EP(Integer acc) {
		var EP0 = new EP();
		EP0.acc = acc;
		
		switch (curToken.type()) {
			case PLUS -> {
				var T2 = new T();
				var EP3 = new EP();
				
				if (curToken.type() != ArithmeticToken.Type.PLUS) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.PLUS, lexer.curPos());
				}
				var PLUS1 = curToken;
				
				Node _PLUS1 = new Node(ArithmeticToken.Type.PLUS);
				_PLUS1.children.add(new Node(PLUS1.text()));
				EP0._node.children.add(_PLUS1);
				curToken = lexer.next();
				
				var _T2 = T();
				T2.val = _T2.val;
				T2._node = _T2._node;
				{ EP3.acc = EP0.acc + T2.val; }
				EP0._node.children.add(T2._node);
				
				var _EP3 = EP(EP3.acc);
				EP3.val = _EP3.val;
				EP3._node = _EP3._node;
				{EP0.val = EP3.val;}
				EP0._node.children.add(EP3._node);
				
			}
			case MINUS -> {
				var T2 = new T();
				var EP3 = new EP();
				
				if (curToken.type() != ArithmeticToken.Type.MINUS) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.MINUS, lexer.curPos());
				}
				var MINUS1 = curToken;
				
				Node _MINUS1 = new Node(ArithmeticToken.Type.MINUS);
				_MINUS1.children.add(new Node(MINUS1.text()));
				EP0._node.children.add(_MINUS1);
				curToken = lexer.next();
				
				var _T2 = T();
				T2.val = _T2.val;
				T2._node = _T2._node;
				{ EP3.acc = EP0.acc - T2.val; }
				EP0._node.children.add(T2._node);
				
				var _EP3 = EP(EP3.acc);
				EP3.val = _EP3.val;
				EP3._node = _EP3._node;
				{EP0.val = EP3.val;}
				EP0._node.children.add(EP3._node);
				
			}
			case _END, COMMA, RPAREN -> {
				
				{EP0.val = EP0.acc;}
				EP0._node.children.add(new Node(""));
				
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return EP0;
	}
	
	public class T {
		public Node _node = new Node("T");
		public Integer val;
		public String text() {
			return _node.toString();
		}
	}
	
	public T T() {
		var T0 = new T();
		
		switch (curToken.type()) {
			case IDENT, NUM, LPAREN, MINUS -> {
				var F1 = new F();
				var TP2 = new TP();
				
				var _F1 = F();
				F1.val = _F1.val;
				F1.call = _F1.call;
				F1.fName = _F1.fName;
				F1._node = _F1._node;
				{TP2.acc = F1.val;}
				T0._node.children.add(F1._node);
				
				var _TP2 = TP(TP2.acc);
				TP2.val = _TP2.val;
				TP2._node = _TP2._node;
				{T0.val = TP2.val;}
				T0._node.children.add(TP2._node);
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return T0;
	}
	
	public class TP {
		public Node _node = new Node("TP");
		public Integer acc;
		public Integer val;
		public String text() {
			return _node.toString();
		}
	}
	
	public TP TP(Integer acc) {
		var TP0 = new TP();
		TP0.acc = acc;
		
		switch (curToken.type()) {
			case MUL -> {
				var F2 = new F();
				var TP3 = new TP();
				
				if (curToken.type() != ArithmeticToken.Type.MUL) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.MUL, lexer.curPos());
				}
				var MUL1 = curToken;
				
				Node _MUL1 = new Node(ArithmeticToken.Type.MUL);
				_MUL1.children.add(new Node(MUL1.text()));
				TP0._node.children.add(_MUL1);
				curToken = lexer.next();
				
				var _F2 = F();
				F2.val = _F2.val;
				F2.call = _F2.call;
				F2.fName = _F2.fName;
				F2._node = _F2._node;
				{TP3.acc = TP0.acc * F2.val;}
				TP0._node.children.add(F2._node);
				
				var _TP3 = TP(TP3.acc);
				TP3.val = _TP3.val;
				TP3._node = _TP3._node;
				{TP0.val = TP3.val;}
				TP0._node.children.add(TP3._node);
				
			}
			case DIV -> {
				var F2 = new F();
				var TP3 = new TP();
				
				if (curToken.type() != ArithmeticToken.Type.DIV) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.DIV, lexer.curPos());
				}
				var DIV1 = curToken;
				
				Node _DIV1 = new Node(ArithmeticToken.Type.DIV);
				_DIV1.children.add(new Node(DIV1.text()));
				TP0._node.children.add(_DIV1);
				curToken = lexer.next();
				
				var _F2 = F();
				F2.val = _F2.val;
				F2.call = _F2.call;
				F2.fName = _F2.fName;
				F2._node = _F2._node;
				{TP3.acc = TP0.acc / F2.val;}
				TP0._node.children.add(F2._node);
				
				var _TP3 = TP(TP3.acc);
				TP3.val = _TP3.val;
				TP3._node = _TP3._node;
				{TP0.val = TP3.val;}
				TP0._node.children.add(TP3._node);
				
			}
			case _END, COMMA, RPAREN, PLUS, MINUS -> {
				
				{TP0.val = TP0.acc;}
				TP0._node.children.add(new Node(""));
				
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return TP0;
	}
	
	public class F {
		public Node _node = new Node("F");
		public Integer val;
		public String call;
		public String fName;
		public String text() {
			return _node.toString();
		}
	}
	
	public F F() {
		var F0 = new F();
		
		switch (curToken.type()) {
			case LPAREN -> {
				var E2 = new E();
				
				if (curToken.type() != ArithmeticToken.Type.LPAREN) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.LPAREN, lexer.curPos());
				}
				var LPAREN1 = curToken;
				
				Node _LPAREN1 = new Node(ArithmeticToken.Type.LPAREN);
				_LPAREN1.children.add(new Node(LPAREN1.text()));
				F0._node.children.add(_LPAREN1);
				curToken = lexer.next();
				
				var _E2 = E();
				E2.val = _E2.val;
				E2._node = _E2._node;
				{F0.val = E2.val;}
				F0._node.children.add(E2._node);
				
				if (curToken.type() != ArithmeticToken.Type.RPAREN) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.RPAREN, lexer.curPos());
				}
				var RPAREN3 = curToken;
				
				Node _RPAREN3 = new Node(ArithmeticToken.Type.RPAREN);
				_RPAREN3.children.add(new Node(RPAREN3.text()));
				F0._node.children.add(_RPAREN3);
				curToken = lexer.next();
				
			}
			case NUM -> {
				
				if (curToken.type() != ArithmeticToken.Type.NUM) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.NUM, lexer.curPos());
				}
				var NUM1 = curToken;
				{F0.val = Integer.parseInt(NUM1.text());}
				Node _NUM1 = new Node(ArithmeticToken.Type.NUM);
				_NUM1.children.add(new Node(NUM1.text()));
				F0._node.children.add(_NUM1);
				curToken = lexer.next();
				
			}
			case MINUS -> {
				var F2 = new F();
				
				if (curToken.type() != ArithmeticToken.Type.MINUS) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.MINUS, lexer.curPos());
				}
				var MINUS1 = curToken;
				
				Node _MINUS1 = new Node(ArithmeticToken.Type.MINUS);
				_MINUS1.children.add(new Node(MINUS1.text()));
				F0._node.children.add(_MINUS1);
				curToken = lexer.next();
				
				var _F2 = F();
				F2.val = _F2.val;
				F2.call = _F2.call;
				F2.fName = _F2.fName;
				F2._node = _F2._node;
				{F0.val = -F2.val;}
				F0._node.children.add(F2._node);
				
			}
			case IDENT -> {
				var E3 = new E();
				var FC4 = new FC();
				
				if (curToken.type() != ArithmeticToken.Type.IDENT) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.IDENT, lexer.curPos());
				}
				var IDENT1 = curToken;
				{F0.fName = IDENT1.text();}
				Node _IDENT1 = new Node(ArithmeticToken.Type.IDENT);
				_IDENT1.children.add(new Node(IDENT1.text()));
				F0._node.children.add(_IDENT1);
				curToken = lexer.next();
				
				if (curToken.type() != ArithmeticToken.Type.LPAREN) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.LPAREN, lexer.curPos());
				}
				var LPAREN2 = curToken;
				
				Node _LPAREN2 = new Node(ArithmeticToken.Type.LPAREN);
				_LPAREN2.children.add(new Node(LPAREN2.text()));
				F0._node.children.add(_LPAREN2);
				curToken = lexer.next();
				
				var _E3 = E();
				E3.val = _E3.val;
				E3._node = _E3._node;
				
				F0._node.children.add(E3._node);
				
				var _FC4 = FC(F0.fName, E3.val);
				FC4.val = _FC4.val;
				FC4._node = _FC4._node;
				{F0.val = FC4.val;}
				F0._node.children.add(FC4._node);
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return F0;
	}
	
	public class FC {
		public Node _node = new Node("FC");
		public String fName;
		public Integer arg1;
		public Integer val;
		public String text() {
			return _node.toString();
		}
	}
	
	public FC FC(String fName, Integer arg1) {
		var FC0 = new FC();
		FC0.fName = fName;
		FC0.arg1 = arg1;
		
		switch (curToken.type()) {
			case COMMA -> {
				var E2 = new E();
				
				if (curToken.type() != ArithmeticToken.Type.COMMA) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.COMMA, lexer.curPos());
				}
				var COMMA1 = curToken;
				
				Node _COMMA1 = new Node(ArithmeticToken.Type.COMMA);
				_COMMA1.children.add(new Node(COMMA1.text()));
				FC0._node.children.add(_COMMA1);
				curToken = lexer.next();
				
				var _E2 = E();
				E2.val = _E2.val;
				E2._node = _E2._node;
				{FC0.val = (int) binaryOps.get(fName).applyAsDouble(arg1, E2.val);}
				FC0._node.children.add(E2._node);
				
				if (curToken.type() != ArithmeticToken.Type.RPAREN) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.RPAREN, lexer.curPos());
				}
				var RPAREN3 = curToken;
				
				Node _RPAREN3 = new Node(ArithmeticToken.Type.RPAREN);
				_RPAREN3.children.add(new Node(RPAREN3.text()));
				FC0._node.children.add(_RPAREN3);
				curToken = lexer.next();
				
			}
			case RPAREN -> {
				
				if (curToken.type() != ArithmeticToken.Type.RPAREN) {
					throw new UnexpectedTokenException(curToken, ArithmeticToken.Type.RPAREN, lexer.curPos());
				}
				var RPAREN1 = curToken;
				{FC0.val = (int) unaryOps.get(fName).applyAsDouble(arg1);}
				Node _RPAREN1 = new Node(ArithmeticToken.Type.RPAREN);
				_RPAREN1.children.add(new Node(RPAREN1.text()));
				FC0._node.children.add(_RPAREN1);
				curToken = lexer.next();
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return FC0;
	}
	
	public class Node {
		public String value;
		
		public List<Node> children;
		
		public Node(String value) {
			this.value = value;
			this.children = new ArrayList<>();
		}
		
		public Node(ArithmeticToken.Type terminal) {
			this(terminal.toString());
		}
		
		@Override
		public String toString() {
			if (children.isEmpty()) {
				return value;
			}
			return children.stream().map(Node::toString).collect(Collectors.joining(" "));
		}
		
	}
	
	public static class UnexpectedTokenException extends RuntimeException {
		public UnexpectedTokenException(ArithmeticToken token, int errIndex) {
			super("Unexpected token: '" + token.type() + "' at index " + errIndex);
		}
		
		public UnexpectedTokenException(ArithmeticToken actual, ArithmeticToken.Type expected, int errIndex) {
			super("Expected token: '" + expected + "', but found '" + actual.type() + "' at index " + (errIndex + 1));
		}
	}
	
	
}