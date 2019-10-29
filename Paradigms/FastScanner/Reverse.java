import java.io.*;
import java.util.*;

public class Reverse {
	
	private static String[] split(String s) {
		int size = 0;
		int i = 0;
		while (i < s.length()) {
			while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
				i++;
			}
			if (i < s.length()) {
				while (i < s.length() && !Character.isWhitespace(s.charAt(i))) {
					i++;
				}
				size++;
			}
		}
		String[] words = new String[size];
		i = 0;
		int pos = 0;
		while (i < s.length()) {
			while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
				i++;
			}
			if (i < s.length()) {
				StringBuilder sb = new StringBuilder();
				while (i < s.length() && !Character.isWhitespace(s.charAt(i))) {
					sb.append(s.charAt(i++));
				}
				words[pos++] = sb.toString();
			}
		}
		return words;
	}
	
	public static void main(String[] args) throws IOException {
		try (FastScanner in = new FastScanner(System.in)) {
			ArrayList<String> a = new ArrayList<>();
			while (in.hasNextLine()) {
				String line = in.readLine();
				ArrayList<Integer> num = new ArrayList<>();
				if (!line.equals("")) {
					String[] splitted = split(line);
					for (String s : splitted) {
						num.add(Integer.parseInt(s));
					}
				}
				Collections.reverse(num);
				StringBuilder sb = new StringBuilder();
				for (int number : num) {
					sb.append(number);
					sb.append(" ");
				}
				a.add(sb.toString());
			}
			for (int i = a.size() - 1; i >= 0; i--) {
				System.out.println(a.get(i));
			}
		} catch (NumberFormatException e) {
			System.out.println("Number format error");
		} catch (FileNotFoundException e) {
			System.out.println("No such file");
		}
	}
	
}
