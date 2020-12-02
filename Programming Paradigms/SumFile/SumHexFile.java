import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class SumHexFile {

	public static void main(String[] args) {
		if (args.length < 2) {
			return;
		}
		try (Scanner in = new Scanner(new File(args[0]), "utf8"); PrintWriter out = new PrintWriter(new File(args[1]))) {
			int sum = 0;
			while (in.hasNext()) {
				String num = in.next().toLowerCase();
				if (num.startsWith("0x")) {
					num = num.substring(2);
					sum += Integer.parseUnsignedInt(num, 16);
				} else {
					sum += Integer.parseInt(num);
				}
			}
			out.println(sum);
		} catch (FileNotFoundException e) {
			System.out.println("File not found");
		} catch (NumberFormatException e) {
			System.out.println("Number format");
		}
	}
}
