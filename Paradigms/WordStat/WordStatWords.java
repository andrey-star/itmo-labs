import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;

public class WordStatWords {

	public static void main(String[] args) {
		if (args != null || args.length < 2) {
			System.out.println("Specify input and output file paths");
			return;
		}
		try {
			String text = getText(args[0]);
			TreeMap<String, Integer> map = stringToTreeMap(text);
			try (PrintWriter out = new PrintWriter(new File(args[1]), "utf8")) {
				for (Map.Entry<String, Integer> pair : map.entrySet()) {
					out.println(pair.getKey() + " " + pair.getValue());
				}
			} catch (FileNotFoundException e) {
				System.out.println("Invalid output file path");
			} catch (UnsupportedEncodingException e) {
				System.out.println("Unsupported encoding");
			}
		} catch (FileNotFoundException e) {
			System.out.println("Invalid input file path");
		}
	}

	private static String getText(String filePath) throws FileNotFoundException {
		Scanner in = new Scanner(new File(filePath), "utf8");
		StringBuilder sb = new StringBuilder();
		while (in.hasNext()) {
			sb.append(in.next());
			sb.append(" ");
		}
		in.close();

		return sb.toString().toLowerCase().replaceAll("[^\\p{L}'\\p{Pd}]", " ").trim();
	}

	private static TreeMap<String, Integer> stringToTreeMap(String text) {
		TreeMap<String, Integer> map = new TreeMap<>();
		if (!text.isEmpty()) {
			String[] words = text.split(" +");
			for (String word : words) {
				if (map.containsKey(word)) {
					map.put(word, map.get(word) + 1);
				} else {
					map.put(word, 1);
				}
			}
		}
		return map;
	}
}
