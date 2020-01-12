import java.io.*;

public class C_Lottery {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("lottery.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		double p = 1;
		double res = 0;
		double lastPrize = 0;
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			double a = 1.0 / Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			res += lastPrize * p * (1 - a);
			p *= a;
			lastPrize = b;
		}
		res += lastPrize * p;
		PrintWriter out = new PrintWriter(new File("lottery.out"));
		out.println(n - res);
		out.close();
	}
	
}
