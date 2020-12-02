package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

public class Md2Html {
	
	public static void main(String[] args) throws IOException {
		if (args == null || args.length != 2) {
			System.out.println("Usage: Md2Html [input file path] [output file path]");
			return;
		}
		String input = args[0];
		String output = args[1];
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(input), StandardCharsets.UTF_8))) {
			Converter converter = new Md2HtmlConverter(reader.lines().collect(Collectors.joining("\n")));
			String res = converter.convert();
			try (PrintWriter out = new PrintWriter(new File(output), StandardCharsets.UTF_8)) {
				out.println(res);
				System.out.println(res);
			} catch (IOException e) {
				System.out.println(String.format("Invalid output path: %s", output));
			}
		} catch (FileNotFoundException e) {
			System.out.println(String.format("Input file not found: %s", input));
		}
	}
	
}
