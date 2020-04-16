import parser.exception.ParsingException;
import parser.expression.Expression;
import parser.MathLogicParser;
import parser.Parser;

import java.util.Scanner;

public class Main {
	
	public static void main(String[] args) throws ParsingException {
		Scanner in = new Scanner(System.in);
		String s = in.nextLine();
		in.close();
		
		Parser parser = new MathLogicParser();
		Expression parsed = parser.parse(s);
		System.out.println(parsed);
	}
	
}
