import java.io.*;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class B_Chvatal {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("chvatal.in")));
		int n = Integer.parseInt(in.readLine());
		boolean[][] g = new boolean[n][n];
		for (int i = 0; i < n; i++) {
			String s = in.readLine();
			for (int j = 0; j < i; j++) {
				if (s.charAt(j) == '1') {
					g[i][j] = true;
					g[j][i] = true;
				}
			}
		}
		
		Deque<Integer> q = new ArrayDeque<>();
		for (int i = 0; i < n; i++) {
			q.addLast(i);
		}
		for (int i = 0; i < n * n - n; i++) {
			int first = q.removeFirst();
			int second = q.getFirst();
			q.addFirst(first);
			if (!g[first][second]) {
				int k = 2;
				int[] a = q.stream().mapToInt(Integer::intValue).toArray();
				while (k < a.length - 1 && (!g[a[0]][a[k]] || !g[a[1]][a[k + 1]])) {
					k++;
				}
				if (k == a.length - 1) {
					k = 2;
					while (!g[a[0]][a[k]]) {
						k++;
					}
				}
				reverse(a, 1, k + 1);
				q = dek(a);
			}
			q.addLast(q.removeFirst());
		}
		PrintWriter out = new PrintWriter(new File("chvatal.out"));
		out.println(q.stream().map(k -> k + 1 + " ").collect(Collectors.joining()));
		out.close();
	}
	
	private static void reverse(int[] a, int i, int j) {
		for (int k = i; k < (i + j) / 2; k++)  {
			int s = j - (k - i) - 1;
			int temp = a[s];
			a[s] = a[k];
			a[k] = temp;
		}
	}
	
	private static Deque<Integer> dek(int[] a) {
		Deque<Integer> aa = new ArrayDeque<>();
		for (int k : a) {
			aa.add(k);
		}
		return aa;
	}
	
}
