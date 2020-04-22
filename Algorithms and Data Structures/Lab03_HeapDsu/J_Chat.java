import java.io.*;

public class J_Chat {
	
	private static class DSU {
		
		private final int[] chat;
		private final int[] rank;
		private final int[] sentToChat;
		private final int[] readByPerson;
		
		private DSU(int n) {
			sentToChat = new int[n];
			readByPerson = new int[n];
			rank = new int[n];
			chat = new int[n];
			for (int i = 0; i < n; i++) {
				chat[i] = i;
			}
		}
		
		private boolean unite(int x, int y) {
			x = find(x);
			y = find(y);
			if (x != y) {
				if (rank[x] == rank[y]) {
					rank[x]++;
				}
				if (rank[x] < rank[y]) {
					chat[x] = y;
					sentToChat[x] -= sentToChat[y];
				} else {
					chat[y] = x;
					sentToChat[y] -= sentToChat[x];
				}
				return true;
			}
			return false;
		}
		
		private void send(int x) {
			sentToChat[find(x)] += 1;
		}
		
		private int sentToPerson(int x) {
			int sent = sentToChat[x];
			if (chat[x] != x) {
				sent += sentToPerson(chat[x]);
			}
			return sent;
		}
		
		private int read(int x) {
			int received = sentToPerson(x);
			int unread = received - readByPerson[x];
			readByPerson[x] = received;
			return unread;
		}
		
		private int find(int index) {
			return chat[index] != index ? find(chat[index]) : index;
		}
	
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int p = (int) 1e6 + 3;
		PrintWriter out = new PrintWriter(System.out);
		DSU dsu = new DSU(n);
		long zerg = 0;
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int t = Integer.parseInt(line[0]);
			if (t == 1) {
				// Send
				int a = Integer.parseInt(line[1]);
				dsu.send((int) ((a + zerg) % n));
				zerg = (30 * zerg + 239) % p;
			} else if (t == 2) {
				// Unite
				int a = Integer.parseInt(line[1]);
				int b = Integer.parseInt(line[2]);
				if (dsu.unite((int) ((a + zerg) % n), (int) ((b + zerg) % n))) {
					zerg = (13 * zerg + 11) % p;
				}
			} else if (t == 3) {
				// Check unread
				int a = Integer.parseInt(line[1]);
				int q = dsu.read((int) ((a + zerg) % n));
				out.println(q);
				zerg = (100500 * zerg + q) % p;
			}
			
		}
		out.close();
	}
	
}
