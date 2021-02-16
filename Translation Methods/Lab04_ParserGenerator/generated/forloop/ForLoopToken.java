package forloop;

public record ForLoopToken(Type type, String text) {
	
	public enum Type {
		FOR, EQ, INC, DEC, LE, GE, GT, LT, LPAREN, RPAREN, SEMICOLON, ASSIGN, VAR, NUM, _END
	}
	
}