import java.io.*;
import java.util.Arrays;
import java.util.Scanner;

public class B_Shooter {
	
	public static void main(String[] args) throws IOException {
		Scanner in = new Scanner(new File("shooter.in"));
		int n = in.nextInt();
		int m = in.nextInt();
		int k = in.nextInt();
		double[] ps = Arrays.stream(new double[n])
				.map(p -> Math.pow(1 - Double.parseDouble(in.next()), m))
				.toArray();
		double sum = Arrays.stream(ps).reduce(0, Double::sum);
		PrintWriter out = new PrintWriter(new File("shooter.out"));
		out.println(ps[k - 1] / sum);
		out.close();
	}
	
}
