package arithmetic;

public record ArithmeticToken(Type type, String text) {
	
	public enum Type {
		PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, COMMA, NUM, IDENT, _END
	}
	
}