import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

public class WordStatInput {

	public static void main(String[] args) throws UnsupportedEncodingException {
		if (args.length < 2) {
			return;
		}
		try {
			String text = getText(args[0]);
			LinkedHashMap<String, Integer> map = stringToHashMap(text);
			PrintWriter out = new PrintWriter(new File(args[1]), "utf8");
			for (Map.Entry<String, Integer> pair : map.entrySet()) {
				out.println(pair.getKey() + " " + pair.getValue());
			}
			out.close();
		} catch (FileNotFoundException e) {
//			e.printStackTrace();
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

		// p{Pd} - Dash punctuation
		// p{L} - Letters of all alphabets
		return sb.toString().toLowerCase().replaceAll("[^\\p{L}'\\p{Pd}]", " ").trim();
	}

	private static LinkedHashMap<String, Integer> stringToHashMap(String text) {
		LinkedHashMap<String, Integer> map = new LinkedHashMap<>();
		String[] words = text.split(" +");
		for (String word : words) {
			if (map.containsKey(word)) {
				map.put(word, map.get(word) + 1);
			} else {
				map.put(word, 1);
			}
		}
		return map;
	}
}
