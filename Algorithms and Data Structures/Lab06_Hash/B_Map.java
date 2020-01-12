import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class B_Map {
	
	private int size;
	private List<Entry>[] el;
	private int p;
	private int[] r = new int[20];
	private Random random;
	private FastScanner in;
	private PrintWriter out;
	
	private B_Map() {
		random = new Random(500);
		p = 11;
		genR();
		//noinspection unchecked
		el = new ArrayList[11];
	}
	
	@SuppressWarnings("unused")
	private static void test(int am, String path) throws FileNotFoundException {
		PrintWriter out = new PrintWriter(new File(path));
		for (int i = 0; i < am; i++) {
			out.println("put " + i + " " + i);
		}
//		for (int i = -50; i < 50; i++) {
//			out.println("exists " + i);
//		}
		out.close();
	}
	
	public static void main(String[] args) throws FileNotFoundException {
//		test((int) 10e4, "map.in");
		new B_Map().run();
	}
	
	private void solve() {
//		Scanner in = new Scanner(System.in);
		B_Map s = new B_Map();
		String cmd;
		String key, value;
		while (true) {
			try {
				cmd = in.next();
			} catch (NullPointerException e) {
				break;
			}
			key = in.next();
			if (cmd.equals("put")) {
				value = in.next();
				s.add(key, value);
			} else if (cmd.equals("delete")) {
				s.remove(key);
			} else {
				out.println(s.get(key));
			}
		}
	}
	
	private void add(String key, String value) {
		int hash = hash(key);
		if (el[hash] == null) {
			el[hash] = new ArrayList<>();
		}
		Entry e;
		for (int i = 0; i < el[hash].size(); i++) {
			e = el[hash].get(i);
			if (e != null && e.key.equals(key)) {
				e.value = value;
				return;
			}
		}
		el[hash].add(new Entry(key, value));
		size++;
		if (size > 10 * el.length) {
			resize();
		}
	}
	
	private void remove(String key) {
		int hash = hash(key);
		if (el[hash] == null) {
			return;
		}
		Entry e;
		for (int i = 0; i < el[hash].size(); i++) {
			e = el[hash].get(i);
			if (e != null && e.key.equals(key)) {
				el[hash].remove(i);
				size--;
				return;
			}
		}
	}
	
	private void resize() {
		//noinspection unchecked
		List<Entry>[] temp = new ArrayList[el.length];
		System.arraycopy(el, 0, temp, 0, el.length);
		p = BigInteger.valueOf(2 * el.length).nextProbablePrime().intValue();
		//noinspection unchecked
		el = new ArrayList[2 * el.length];
		size = 0;
		genR();
		//noinspection ForLoopReplaceableByForEach
		for (int i = 0; i < temp.length; i++) {
			if (temp[i] != null) {
				for (int j = 0; j < temp[i].size(); j++) {
					add(temp[i].get(j).key, temp[i].get(j).value);
				}
			}
		}
	}
	
	private String get(String key) {
		int hash = hash(key);
		if (el[hash] == null) {
			return "none";
		}
		Entry e;
		for (int i = 0; i < el[hash].size(); i++) {
			e = el[hash].get(i);
			if (e != null && e.key.equals(key)) {
				return e.value;
			}
		}
		return "none";
	}
	
	private void genR() {
		for (int i = 0; i < r.length; i++) {
			r[i] = random.nextInt(p);
		}
	}
	
	/*
		ints 1e6:
		smart:
		Used Memory: 309 mb
		Total: 3.089 s
	
		java:
		Used Memory: 226 mb
		Total: 2.24 s
		
		bad java hash 3.3e6:
		smart:
		Used Memory: 900 mb
		Total: 11.902 s
		
		java:
		Used Memory: 653 mb
		Total: 81.746 s
		
	 */
	
	private int hash(String key) {
		int hash = 0;
		for (int i = 0; i < r.length; i++) {
			if (i < key.length()) {
				hash += key.charAt(i) * r[i];
			} else {
				hash += r[i];
			}
		}
		hash %= el.length;
//		int hash = key.hashCode() % el.length;
		if (hash < 0) {
			hash += el.length;
		}
		return hash;
	}
	
	private void run() {
//		long start = System.currentTimeMillis();
		try {
			in = new FastScanner(new File("map.in"));
			out = new PrintWriter(new File("map.out"));
			solve();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
//		int mb = 1024 * 1024;
//		Runtime runtime = Runtime.getRuntime();
//		System.out.println("Used Memory: " + (runtime.totalMemory() - runtime.freeMemory()) / mb + " mb");
//		System.out.println("Total: " + (System.currentTimeMillis() - start) / 1000.0 + " s");
	}
	
	class Entry {
		String key;
		String value;
		
		Entry(String key, String value) {
			this.key = key;
			this.value = value;
		}
		
		@Override
		public String toString() {
			return key + ":" + value;
		}
	}
	
	class FastScanner {
		BufferedReader br;
		StringTokenizer st;
		
		FastScanner(File f) {
			try {
				br = new BufferedReader(new FileReader(f));
			} catch (FileNotFoundException e) {
				e.printStackTrace();
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
		
	}
	
}

