import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class C_LinkedMap {
	
	private int size;
	private List<Entry>[] el;
	private int p;
	private int[] r = new int[20];
	private Random random;
	private FastScanner in;
	private PrintWriter out;
	private Entry first, last;
	
	private C_LinkedMap() {
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
//		test((int) 2e5, "linkedmap.in");
		new C_LinkedMap().run();
	}
	
	private void solve() {
//		Scanner in = new Scanner(System.in);
		C_LinkedMap s = new C_LinkedMap();
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
			} else if (cmd.equals("prev")) {
				out.println(s.prev(key));
			} else if (cmd.equals("next")) {
				out.println(s.next(key));
			} else if (cmd.equals("get")) {
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
		if (last != null) {
			last.next = el[hash].get(el[hash].size() - 1);
			el[hash].get(el[hash].size() - 1).prev = last;
			last = el[hash].get(el[hash].size() - 1);
		}
		if (first == null) {
			first = el[hash].get(el[hash].size() - 1);
			last = first;
		}
		size++;
		if (size > 5 * el.length) {
			resize();
		}
	}
	
	private void remove(String key) {
		int hash = hash(key);
		if (el[hash] == null || el[hash].isEmpty()) {
			return;
		}
		Entry e;
		for (int i = 0; i < el[hash].size(); i++) {
			e = el[hash].get(i);
			if (e != null && e.key.equals(key)) {
				if (size == 1) {
					first = null;
					last = null;
					el[hash].remove(i);
					size--;
					return;
				}
				if (last.key.equals(key)) {
					last.prev.next = null;
					last = last.prev;
					el[hash].remove(i);
					size--;
					return;
				}
				if (first.key.equals(key)) {
					first.next.prev = null;
					first = first.next;
					el[hash].remove(i);
					size--;
					return;
				}
				e.prev.next = e.next;
				e.next.prev = e.prev;
				el[hash].remove(i);
				size--;
				return;
			}
		}
	}
	
	private String prev(String key) {
		Entry e = getEntry(key);
		if (e == null) {
			return "none";
		}
		e = e.prev;
		if (e == null) {
			return "none";
		}
		return e.value;
	}
	
	private String next(String key) {
		Entry e = getEntry(key);
		if (e == null) {
			return "none";
		}
		e = e.next;
		if (e == null) {
			return "none";
		}
		return e.value;
	}
	
	private void resize() {
		p = BigInteger.valueOf(2 * el.length).nextProbablePrime().intValue();
		//noinspection unchecked
		el = new ArrayList[2 * el.length];
		size = 0;
		genR();
		Entry e = first;
		first = null;
		last = null;
		while (e != null) {
			add(e.key, e.value);
			e = e.next;
		}
	}
	
	private String get(String key) {
		Entry e = getEntry(key);
		return e == null ? "none" : e.value;
	}
	
	private Entry getEntry(String key) {
		int hash = hash(key);
		if (el[hash] == null) {
			return null;
		}
		Entry e;
		for (int i = 0; i < el[hash].size(); i++) {
			e = el[hash].get(i);
			if (e != null && e.key.equals(key)) {
				return e;
			}
		}
		return null;
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
		long start = System.currentTimeMillis();
		try {
			in = new FastScanner(new File("linkedmap.in"));
			out = new PrintWriter(new File("linkedmap.out"));
			solve();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		int mb = 1024 * 1024;
		Runtime runtime = Runtime.getRuntime();
		System.out.println("Used Memory: " + (runtime.totalMemory() - runtime.freeMemory()) / mb + " mb");
		System.out.println("Total: " + (System.currentTimeMillis() - start) / 1000.0 + " s");
	}
	
	class Entry {
		String key;
		String value;
		Entry prev;
		Entry next;
		
		Entry(String key, String value) {
			this.key = key;
			this.value = value;
		}
		
		@Override
		public String toString() {
			return key + ":" + value + " prev=" + (prev == null ? "none" : prev.key)  + " next=" + (next == null ? "none" : next.key);
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

