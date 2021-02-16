package forloop;

import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;


public class ForLoopParser {
	
	ForLoopLexer lexer;
	ForLoopToken curToken;
	
	public ForLoopParser(ForLoopLexer lexer) {
		this.lexer = lexer;
		curToken = this.lexer.next();
	}
	
	public class S {
		public Node _node = new Node("S");
		public String val;
		public String text() {
			return _node.toString();
		}
	}
	
	public S S() {
		var S0 = new S();
		
		switch (curToken.type()) {
			case FOR -> {
				var T3 = new T();
				
				if (curToken.type() != ForLoopToken.Type.FOR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.FOR, lexer.curPos());
				}
				var FOR1 = curToken;
				{S0.val = "for";}
				Node _FOR1 = new Node(ForLoopToken.Type.FOR);
				_FOR1.children.add(new Node(FOR1.text()));
				S0._node.children.add(_FOR1);
				curToken = lexer.next();
				
				if (curToken.type() != ForLoopToken.Type.LPAREN) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.LPAREN, lexer.curPos());
				}
				var LPAREN2 = curToken;
				{S0.val += " (";}
				Node _LPAREN2 = new Node(ForLoopToken.Type.LPAREN);
				_LPAREN2.children.add(new Node(LPAREN2.text()));
				S0._node.children.add(_LPAREN2);
				curToken = lexer.next();
				
				var _T3 = T();
				T3.val = _T3.val;
				T3._node = _T3._node;
				{S0.val += T3.val;}
				S0._node.children.add(T3._node);
				
				if (curToken.type() != ForLoopToken.Type.RPAREN) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.RPAREN, lexer.curPos());
				}
				var RPAREN4 = curToken;
				{S0.val += ")";}
				Node _RPAREN4 = new Node(ForLoopToken.Type.RPAREN);
				_RPAREN4.children.add(new Node(RPAREN4.text()));
				S0._node.children.add(_RPAREN4);
				curToken = lexer.next();
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return S0;
	}
	
	public class T {
		public Node _node = new Node("T");
		public String val;
		public String text() {
			return _node.toString();
		}
	}
	
	public T T() {
		var T0 = new T();
		
		switch (curToken.type()) {
			case VAR -> {
				var C6 = new C();
				var H8 = new H();
				
				if (curToken.type() != ForLoopToken.Type.VAR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.VAR, lexer.curPos());
				}
				var VAR1 = curToken;
				{T0.val = VAR1.text();}
				Node _VAR1 = new Node(ForLoopToken.Type.VAR);
				_VAR1.children.add(new Node(VAR1.text()));
				T0._node.children.add(_VAR1);
				curToken = lexer.next();
				
				if (curToken.type() != ForLoopToken.Type.VAR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.VAR, lexer.curPos());
				}
				var VAR2 = curToken;
				{T0.val += " " + VAR2.text();}
				Node _VAR2 = new Node(ForLoopToken.Type.VAR);
				_VAR2.children.add(new Node(VAR2.text()));
				T0._node.children.add(_VAR2);
				curToken = lexer.next();
				
				if (curToken.type() != ForLoopToken.Type.ASSIGN) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.ASSIGN, lexer.curPos());
				}
				var ASSIGN3 = curToken;
				{T0.val += " = ";}
				Node _ASSIGN3 = new Node(ForLoopToken.Type.ASSIGN);
				_ASSIGN3.children.add(new Node(ASSIGN3.text()));
				T0._node.children.add(_ASSIGN3);
				curToken = lexer.next();
				
				if (curToken.type() != ForLoopToken.Type.NUM) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.NUM, lexer.curPos());
				}
				var NUM4 = curToken;
				{T0.val += NUM4.text();}
				Node _NUM4 = new Node(ForLoopToken.Type.NUM);
				_NUM4.children.add(new Node(NUM4.text()));
				T0._node.children.add(_NUM4);
				curToken = lexer.next();
				
				if (curToken.type() != ForLoopToken.Type.SEMICOLON) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.SEMICOLON, lexer.curPos());
				}
				var SEMICOLON5 = curToken;
				{T0.val += "; ";}
				Node _SEMICOLON5 = new Node(ForLoopToken.Type.SEMICOLON);
				_SEMICOLON5.children.add(new Node(SEMICOLON5.text()));
				T0._node.children.add(_SEMICOLON5);
				curToken = lexer.next();
				
				var _C6 = C();
				C6.val = _C6.val;
				C6._node = _C6._node;
				{T0.val += C6.val;}
				T0._node.children.add(C6._node);
				
				if (curToken.type() != ForLoopToken.Type.SEMICOLON) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.SEMICOLON, lexer.curPos());
				}
				var SEMICOLON7 = curToken;
				{T0.val += "; ";}
				Node _SEMICOLON7 = new Node(ForLoopToken.Type.SEMICOLON);
				_SEMICOLON7.children.add(new Node(SEMICOLON7.text()));
				T0._node.children.add(_SEMICOLON7);
				curToken = lexer.next();
				
				var _H8 = H();
				H8.val = _H8.val;
				H8._node = _H8._node;
				{T0.val += H8.val;}
				T0._node.children.add(H8._node);
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return T0;
	}
	
	public class C {
		public Node _node = new Node("C");
		public String val;
		public String text() {
			return _node.toString();
		}
	}
	
	public C C() {
		var C0 = new C();
		
		switch (curToken.type()) {
			case VAR -> {
				var CMP2 = new CMP();
				
				if (curToken.type() != ForLoopToken.Type.VAR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.VAR, lexer.curPos());
				}
				var VAR1 = curToken;
				{C0.val = VAR1.text();}
				Node _VAR1 = new Node(ForLoopToken.Type.VAR);
				_VAR1.children.add(new Node(VAR1.text()));
				C0._node.children.add(_VAR1);
				curToken = lexer.next();
				
				var _CMP2 = CMP();
				CMP2.val = _CMP2.val;
				CMP2._node = _CMP2._node;
				{C0.val += " " + CMP2.val + " ";}
				C0._node.children.add(CMP2._node);
				
				if (curToken.type() != ForLoopToken.Type.NUM) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.NUM, lexer.curPos());
				}
				var NUM3 = curToken;
				{C0.val += NUM3.text();}
				Node _NUM3 = new Node(ForLoopToken.Type.NUM);
				_NUM3.children.add(new Node(NUM3.text()));
				C0._node.children.add(_NUM3);
				curToken = lexer.next();
				
			}
			case NUM -> {
				var CMP2 = new CMP();
				
				if (curToken.type() != ForLoopToken.Type.NUM) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.NUM, lexer.curPos());
				}
				var NUM1 = curToken;
				{C0.val = NUM1.text();}
				Node _NUM1 = new Node(ForLoopToken.Type.NUM);
				_NUM1.children.add(new Node(NUM1.text()));
				C0._node.children.add(_NUM1);
				curToken = lexer.next();
				
				var _CMP2 = CMP();
				CMP2.val = _CMP2.val;
				CMP2._node = _CMP2._node;
				{C0.val += " " + CMP2.val + " ";}
				C0._node.children.add(CMP2._node);
				
				if (curToken.type() != ForLoopToken.Type.VAR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.VAR, lexer.curPos());
				}
				var VAR3 = curToken;
				{C0.val += VAR3.text();}
				Node _VAR3 = new Node(ForLoopToken.Type.VAR);
				_VAR3.children.add(new Node(VAR3.text()));
				C0._node.children.add(_VAR3);
				curToken = lexer.next();
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return C0;
	}
	
	public class CMP {
		public Node _node = new Node("CMP");
		public String val;
		public String text() {
			return _node.toString();
		}
	}
	
	public CMP CMP() {
		var CMP0 = new CMP();
		
		switch (curToken.type()) {
			case LT -> {
				
				if (curToken.type() != ForLoopToken.Type.LT) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.LT, lexer.curPos());
				}
				var LT1 = curToken;
				{CMP0.val = "<";}
				Node _LT1 = new Node(ForLoopToken.Type.LT);
				_LT1.children.add(new Node(LT1.text()));
				CMP0._node.children.add(_LT1);
				curToken = lexer.next();
				
			}
			case GT -> {
				
				if (curToken.type() != ForLoopToken.Type.GT) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.GT, lexer.curPos());
				}
				var GT1 = curToken;
				{CMP0.val = ">";}
				Node _GT1 = new Node(ForLoopToken.Type.GT);
				_GT1.children.add(new Node(GT1.text()));
				CMP0._node.children.add(_GT1);
				curToken = lexer.next();
				
			}
			case LE -> {
				
				if (curToken.type() != ForLoopToken.Type.LE) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.LE, lexer.curPos());
				}
				var LE1 = curToken;
				{CMP0.val = "<=";}
				Node _LE1 = new Node(ForLoopToken.Type.LE);
				_LE1.children.add(new Node(LE1.text()));
				CMP0._node.children.add(_LE1);
				curToken = lexer.next();
				
			}
			case GE -> {
				
				if (curToken.type() != ForLoopToken.Type.GE) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.GE, lexer.curPos());
				}
				var GE1 = curToken;
				{CMP0.val = ">=";}
				Node _GE1 = new Node(ForLoopToken.Type.GE);
				_GE1.children.add(new Node(GE1.text()));
				CMP0._node.children.add(_GE1);
				curToken = lexer.next();
				
			}
			case EQ -> {
				
				if (curToken.type() != ForLoopToken.Type.EQ) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.EQ, lexer.curPos());
				}
				var EQ1 = curToken;
				{CMP0.val = "==";}
				Node _EQ1 = new Node(ForLoopToken.Type.EQ);
				_EQ1.children.add(new Node(EQ1.text()));
				CMP0._node.children.add(_EQ1);
				curToken = lexer.next();
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return CMP0;
	}
	
	public class H {
		public Node _node = new Node("H");
		public String val;
		public String text() {
			return _node.toString();
		}
	}
	
	public H H() {
		var H0 = new H();
		
		switch (curToken.type()) {
			case VAR -> {
				var SHIFT2 = new SHIFT();
				
				if (curToken.type() != ForLoopToken.Type.VAR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.VAR, lexer.curPos());
				}
				var VAR1 = curToken;
				{H0.val = VAR1.text();}
				Node _VAR1 = new Node(ForLoopToken.Type.VAR);
				_VAR1.children.add(new Node(VAR1.text()));
				H0._node.children.add(_VAR1);
				curToken = lexer.next();
				
				var _SHIFT2 = SHIFT();
				SHIFT2.val = _SHIFT2.val;
				SHIFT2._node = _SHIFT2._node;
				{H0.val += SHIFT2.val;}
				H0._node.children.add(SHIFT2._node);
				
			}
			case DEC, INC -> {
				var SHIFT1 = new SHIFT();
				
				var _SHIFT1 = SHIFT();
				SHIFT1.val = _SHIFT1.val;
				SHIFT1._node = _SHIFT1._node;
				{H0.val = SHIFT1.val;}
				H0._node.children.add(SHIFT1._node);
				
				if (curToken.type() != ForLoopToken.Type.VAR) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.VAR, lexer.curPos());
				}
				var VAR2 = curToken;
				{H0.val += VAR2.text();}
				Node _VAR2 = new Node(ForLoopToken.Type.VAR);
				_VAR2.children.add(new Node(VAR2.text()));
				H0._node.children.add(_VAR2);
				curToken = lexer.next();
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return H0;
	}
	
	public class SHIFT {
		public Node _node = new Node("SHIFT");
		public String val;
		public String text() {
			return _node.toString();
		}
	}
	
	public SHIFT SHIFT() {
		var SHIFT0 = new SHIFT();
		
		switch (curToken.type()) {
			case INC -> {
				
				if (curToken.type() != ForLoopToken.Type.INC) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.INC, lexer.curPos());
				}
				var INC1 = curToken;
				{SHIFT0.val = "++";}
				Node _INC1 = new Node(ForLoopToken.Type.INC);
				_INC1.children.add(new Node(INC1.text()));
				SHIFT0._node.children.add(_INC1);
				curToken = lexer.next();
				
			}
			case DEC -> {
				
				if (curToken.type() != ForLoopToken.Type.DEC) {
					throw new UnexpectedTokenException(curToken, ForLoopToken.Type.DEC, lexer.curPos());
				}
				var DEC1 = curToken;
				{SHIFT0.val = "--";}
				Node _DEC1 = new Node(ForLoopToken.Type.DEC);
				_DEC1.children.add(new Node(DEC1.text()));
				SHIFT0._node.children.add(_DEC1);
				curToken = lexer.next();
				
			}
			default -> throw new UnexpectedTokenException(curToken, lexer.curPos());
		}
		
		return SHIFT0;
	}
	
	public class Node {
		public String value;
		
		public List<Node> children;
		
		public Node(String value) {
			this.value = value;
			this.children = new ArrayList<>();
		}
		
		public Node(ForLoopToken.Type terminal) {
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
		public UnexpectedTokenException(ForLoopToken token, int errIndex) {
			super("Unexpected token: '" + token.type() + "' at index " + errIndex);
		}
		
		public UnexpectedTokenException(ForLoopToken actual, ForLoopToken.Type expected, int errIndex) {
			super("Expected token: '" + expected + "', but found '" + actual.type() + "' at index " + (errIndex + 1));
		}
	}
	
	
}