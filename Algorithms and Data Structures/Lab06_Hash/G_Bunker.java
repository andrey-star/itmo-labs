import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class G_Bunker {
	
	private int[] r;
	private int p;
	private FastScanner in;
	private PrintWriter out;
	
	public static void main(String[] arg) {
		new G_Bunker().run();
	}
	
	private void test() throws FileNotFoundException {
		int n = (int) 7e4;
		PrintWriter out = new PrintWriter(new File("b.txt"));
		out.println(n);
		for (int i = 0; i < n - 1; i++) {
			out.println((i + 1) + " " + (i + 2));
		}
		out.close();
	}
	
	private void solve() {
		int n = in.nextInt();
		if (n == 1) {
			out.println("NO");
			return;
		}
		p = 2143920257;
		//noinspection unchecked
		ArrayList<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n - 1; i++) {
			int u = in.nextInt() - 1;
			int v = in.nextInt() - 1;
			g[u].add(v);
			g[v].add(u);
		}
		int root = getRoot(g);
		if (root == -1) {
			out.println("NO");
		} else {
			r = new int[n];
			Random random = new Random(102);
			for (int i = 0; i < r.length; i++) {
				r[i] = random.nextInt(p);
			}
			out.println(isomorphic(g, g[root].get(0), g[root].get(1), root) ? "YES" : "NO");
		}
	}
	
	private boolean isomorphic(ArrayList<Integer>[] g, int a, int b, int p) {
		Map map = new Map();
		int first = type(g, a, p, map);
		int second = type(g, b, p, map);
		return first == second;
	}
	
	private int type(ArrayList<Integer>[] g, int a, int p, Map map) {
		if (g[a].size() == 1) {
			return 1;
		}
		int[] chTypes = new int[g[a].size()];
		for (int i = 0; i < chTypes.length; i++) {
			if (g[a].get(i) != p) {
				chTypes[i] = type(g, g[a].get(i), a, map);
			}
		}
		Arrays.sort(chTypes);
		return map.get(hash(chTypes));
	}
	
	private int getRoot(ArrayList<Integer>[] g) {
		int root = dfsRec(g, 0, -1, new int[g.length]);
		if (root == -1) {
			return -1;
		}
		return g[root].size() == 2 ? root : -1;
	}
	
	private int dfsRec(ArrayList<Integer>[] g, int u, int p, int[] size) {
		size[u] = 1;
		for (int v : g[u]) {
			if (v != p) {
				int res = dfsRec(g, v, u, size);
				if (res != -1) {
					return res;
				}
				if (size[v] == g.length / 2) {
					return u;
				}
				size[u] += size[v];
			}
		}
		return -1;
	}
	
	private int hash(int[] key) {
		int hash = 0;
		for (int i = 0; i < r.length; i++) {
			if (i < key.length) {
				hash += key[i] * r[i];
			}
		}
		hash %= p;
		if (hash < 0) {
			hash += p;
		}
		return hash;
	}
	
	private void run() {
		in = new FastScanner(System.in);
		out = new PrintWriter(System.out);
		solve();
		out.close();
	}
	
	static class Map {
		private int lastNotUsed;
		private List<Entry>[] el;
		private int a, b, p;
		private final Random random;
		private int size;
		
		private Map() {
			random = new Random(500);
			size = 0;
			p = (int) 1e9 + 9;
//			p = 11;
			a = random.nextInt(p);
			b = random.nextInt(p);
			lastNotUsed = 2;
			//noinspection unchecked
			el = new ArrayList[(int) 1e6];
//			el = new ArrayList[11];
		}
		
		private void add(int key, int value) {
			int hash = hash(key);
			if (el[hash] == null) {
				el[hash] = new ArrayList<>();
			}
			Entry e;
			for (int i = 0; i < el[hash].size(); i++) {
				e = el[hash].get(i);
				if (e != null && e.key == key) {
					e.value = value;
					return;
				}
			}
			el[hash].add(new Entry(key, value));
			size++;
//			if (size > el.length * 5) {
//				resize();
//			}
		}
		
		private void resize() {
			//noinspection unchecked
			List<Entry>[] temp = new ArrayList[el.length];
			System.arraycopy(el, 0, temp, 0, el.length);
			p = BigInteger.valueOf(2 * el.length).nextProbablePrime().intValue();
			//noinspection unchecked
			el = new ArrayList[2 * el.length];
			size = 0;
			a = random.nextInt(p);
			b = random.nextInt(p);
			//noinspection ForLoopReplaceableByForEach
			for (int i = 0; i < temp.length; i++) {
				if (temp[i] != null) {
					for (int j = 0; j < temp[i].size(); j++) {
						add(temp[i].get(j).key, temp[i].get(j).value);
					}
				}
			}
		}
		
		private int get(int key) {
			int hash = hash(key);
			if (el[hash] == null) {
				add(key, lastNotUsed);
				return lastNotUsed++;
			}
			Entry e;
			for (int i = 0; i < el[hash].size(); i++) {
				e = el[hash].get(i);
				if (e != null && e.key == key) {
					return e.value;
				}
			}
			add(key, lastNotUsed);
			return lastNotUsed++;
		}
		
		private int hash(int key) {
			int hash = (int) (((long) a * key + b) % p) % el.length;
			if (hash < 0) {
				hash += el.length;
			}
			return hash;
		}
		
		@Override
		public String toString() {
			return Arrays.toString(el);
		}
		
		class Entry {
			int key;
			int value;
			
			Entry(int key, int value) {
				this.key = key;
				this.value = value;
			}
			
			@Override
			public String toString() {
				return key + ":" + value;
			}
		}
	}
	
	static class FastScanner {
		BufferedReader br;
		StringTokenizer st;
		
		FastScanner(File f) {
			try {
				br = new BufferedReader(new FileReader(f));
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
		}
		
		FastScanner(InputStream f) {
			br = new BufferedReader(new InputStreamReader(f));
		}
		
		String nextLine() {
			try {
				return br.readLine();
			} catch (IOException e) {
				return null;
			}
		}
		
		String next() {
			while (st == null || !st.hasMoreTokens()) {
				try {
					st = new StringTokenizer(br.readLine());
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			return st.nextToken();
		}
		
		int nextInt() {
			return Integer.parseInt(next());
		}
		
		long nextLong() {
			return Long.parseLong(next());
		}
		
		double nextDouble() {
			return Double.parseDouble(next());
		}
		
	}
	
}
