import java.io.*;
import java.util.*;

public class Reverse {

	public static void main(String[] args) throws IOException, InterruptedException {
		try (BufferedReader br = new BufferedReader(new InputStreamReader(System.in))) {
			ArrayList<String> a = new ArrayList<>();
			String line;
			while ((line = br.readLine()) != null) {
				System.err.println(line);
				ArrayList<Integer> num = new ArrayList<>();
				if (!line.equals("")) {
					for (String s : line.split(" ")) {
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
		}
		Thread.sleep(5000);

	}

}
