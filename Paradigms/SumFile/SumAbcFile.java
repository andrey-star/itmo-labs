import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class SumAbcFile {
	public static void main(String[] args) {
		if (args != null && args.length < 2) {
			System.out.println("Specify input and output file paths");
			return;
		}

		try (Scanner in = new Scanner(new File(args[0]), "utf8")) {
			int sum = 0;
			while (in.hasNext()) {
				String num = in.next().toLowerCase();
				if (num.replaceAll("[+-]?[a-j]+", "").isEmpty()) {
					for (char c = 'a'; c <= 'j'; c++) {
						num = num.replaceAll("" + c, c - 'a' + "");
					}
					sum += Integer.parseInt(num);
				} else {
					throw new NumberFormatException();
				}
			}
			try (PrintWriter out = new PrintWriter(new File(args[1]))) {
				out.println(sum);
			} catch (FileNotFoundException e) {
				System.out.println("Invalid output file path");
			}
		} catch (FileNotFoundException e1) {
			System.out.println("Invalid input file path");
		} catch (NumberFormatException e) {
			System.out.println("Invalid number format");
		}
	}
}
