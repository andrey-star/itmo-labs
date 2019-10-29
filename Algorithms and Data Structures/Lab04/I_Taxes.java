import java.io.*;
import java.util.ArrayList;

public class I_Taxes {
	
	static class Pair {
		int x;
		int y;
		
		Pair(int x, int y) {
			this.x = x;
			this.y = y;
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("treedp.in")));
		int n = Integer.parseInt(in.readLine());
		//noinspection unchecked
		ArrayList<Integer>[] g = new ArrayList[n];
		Pair[] sides = new Pair[n - 1];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n - 1; i++) {
			String[] line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
			g[b].add(a);
			sides[i] = new Pair(a, b);
		}
		in.close();
		
		PrintWriter out = new PrintWriter(new File("treedp.out"));
		out.close();
		
	}
	
}