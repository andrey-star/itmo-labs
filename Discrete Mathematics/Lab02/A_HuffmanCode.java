import java.io.*;
import java.util.PriorityQueue;

public class A_HuffmanCode {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("huffman.in")));
		int n = Integer.parseInt(in.readLine());
		String[] line = in.readLine().trim().split(" ");
		PriorityQueue<Long> p = new PriorityQueue<>();
		for (int i = 0; i < n; i++) {
			p.add((long) Integer.parseInt(line[i]));
		}
		long ans = 0;
		while (p.size() > 1) {
			long sum = p.remove() + p.remove();
			ans += sum;
			p.add(sum);
		}
		PrintWriter out = new PrintWriter(new File("huffman.out"));
		out.println(ans);
		out.close();
		in.close();
	}
	
}
