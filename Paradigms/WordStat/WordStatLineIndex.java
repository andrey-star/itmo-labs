import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class WordStatLineIndex {
	
	public static void main(String[] args) {
		if (args == null || args.length < 2) {
			System.out.println("Specify input and output files");
			return;
		}
		try {
			TreeMap<String, List<Pair>> map = readToMap(args[0]);
			try (PrintWriter out = new PrintWriter(new File(args[1]), "utf8")) {
				for (Map.Entry<String, List<Pair>> pair : map.entrySet()) {
					out.println(pair.getKey() + " " + pair.getValue().size() + " " + pair
							.getValue()
							.stream()
							.map(val -> val.line + ":" + val.index).collect(Collectors.joining(" ")));
				}
			} catch (FileNotFoundException e) {
				System.out.println("Specify output file");
			} catch (UnsupportedEncodingException e) {
				System.out.println("Unsupported encoding");
			}
			
		} catch (IOException e) {
			System.out.println("Input file not found");
		}
	}
	
	private static TreeMap<String, List<Pair>> readToMap(String filePath) throws IOException {
		List<String> lines = new ArrayList<>();
		FastScanner in = new FastScanner(new FileInputStream(filePath));
		while (in.hasNextLine()) {
			lines.add(in.readWordLine());
		}
		in.close();
		String[] text = lines.toArray(new String[0]);
		TreeMap<String, List<Pair>> map = new TreeMap<>();
		for (int line = 0; line < text.length; line++) {
			String[] words = in.split(text[line]);
			for (int i = 0; i < words.length; i++) {
				String word = words[i];
				List<Pair> indices = map.getOrDefault(word, new ArrayList<>());
				indices.add(new Pair(line + 1, i + 1));
				map.put(word, indices);
			}
		}
		return map;
	}
	
	static class Pair {
		int line;
		int index;
		
		Pair(int line, int index) {
			this.line = line;
			this.index = index;
		}
	}
	
}
