import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class SumFile {
	public static void main(String[] args) {
		if (args.length < 2) {
			return;
		}
		try (Scanner in = new Scanner(new File(args[0]), "utf8"); PrintWriter out = new PrintWriter(new File(args[1]))) {
			int sum = 0;
			while (in.hasNextInt()) {
				sum += in.nextInt();
			}

			out.println(sum);
		} catch (FileNotFoundException e) {
//            e.printStackTrace();
		}
	}

}
