import java.io.*;
import java.util.*;

public class WordStatIndex {
	
	public static void main(String[] args) throws IOException {
		if (args.length < 2) {
			return;
		}
		try {
			LinkedHashMap<String, List<Integer>> map = convert(args[0]);
			PrintWriter out = new PrintWriter(new File(args[1]), "utf8");
			for (String key : map.keySet()) {
				out.print(key + " ");
				List<Integer> indices = map.get(key);
				out.print(indices.size() + " ");
				for (int i = 0; i < indices.size(); i++) {
					out.print(indices.get(i));
					if (i < indices.size() - 1) {
						out.print(" ");
					}
				}
				out.println();
			}
			
			out.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
	
	private static LinkedHashMap<String, List<Integer>> convert(String filePath) throws IOException {
		FastScanner in = new FastScanner(new FileInputStream(filePath));
		StringBuilder sb = new StringBuilder();
		while (in.hasNextLine()) {
			String s = in.readWordLine();
			sb.append(s);
			sb.append("\n");
		}
		in.close();
		
		// p{Pd} - Dash punctuation
		// p{L} - Letters of all alphabets
		String text = sb.toString().toLowerCase().replaceAll("[^\\p{L}'\\p{Pd}]", " ").trim();
		
		LinkedHashMap<String, List<Integer>> map = new LinkedHashMap<>();
		String[] words = in.split(text);
		for (int i = 0; i < words.length; i++) {
			String word = words[i];
			if (map.containsKey(word)) {
				List<Integer> indices = map.get(word);
				indices.add(i + 1);
				map.put(word, indices);
			} else {
				List<Integer> indices = new ArrayList<>();
				indices.add(i + 1);
				map.put(word, indices);
			}
		}
		return map;
	}
	
}