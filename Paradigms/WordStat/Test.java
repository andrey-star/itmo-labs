import java.io.*;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;

public class Test {

	public static void main(String[] args) throws IOException {
		FastScanner in = new FastScanner(new FileInputStream("test.txt"));
		while (in.hasNextLine()) {
			System.out.println(in.readWordLine());
		}
		
	}

}
