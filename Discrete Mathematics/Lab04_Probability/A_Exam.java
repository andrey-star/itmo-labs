import java.io.*;

public class A_Exam {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("exam.in")));
		String[] line = in.readLine().trim().split(" +");
		int k = Integer.parseInt(line[0]);
		int n = Integer.parseInt(line[1]);
		int sum = 0;
		for (int i = 0; i < k; i++) {
			line = in.readLine().trim().split(" +");
			int p = Integer.parseInt(line[0]);
			int m = Integer.parseInt(line[1]);
			sum += p * m;
		}
		PrintWriter out = new PrintWriter(new File("exam.out"));
		out.println(sum / 100.0 / n);
		out.close();
	}
	
}
