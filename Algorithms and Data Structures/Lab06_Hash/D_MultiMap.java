import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class D_MultiMap {
	private FastScanner in;
	private PrintWriter out;
	
	@SuppressWarnings("unused")
	private static void test(int am, String path) throws FileNotFoundException {
		PrintWriter out = new PrintWriter(new File(path));
		for (int i = 0; i < am; i++) {
			out.println("put " + i + " " + i);
		}
//		}
		out.close();
	}
	
	public static void main(String[] args) throws FileNotFoundException {
//		test((int) 3e5, "multimap.in");
		new D_MultiMap().run();
	}
	
	private void solve() {
//		Scanner in = new Scanner(System.in);
		Map s = new Map();
		String cmd;
		String x, y;
		while (true) {
			try {
				cmd = in.next();
			} catch (NullPointerException e) {
				break;
			}
			x = in.next();
			if (cmd.equals("put")) {
				y = in.next();
				s.add(x, y);
			} else if (cmd.equals("delete")) {
				y = in.next();
				s.removePair(x, y);
			} else if (cmd.equals("deleteall")) {
				s.removeAll(x);
			} else {
				out.println(s.get(x));
			}
		}
	}
	
	private void run() {
//		long start = System.currentTimeMillis();
		try {
			in = new FastScanner(new File("multimap.in"));
			out = new PrintWriter(new File("multimap.out"));
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
	
	private class Map {
		private int size;
		private List<Entry>[] el;
		private int p;
		private final int[] r = new int[20];
		private final Random random;
		
		private Map() {
			random = new Random(500);
			p = 11;
			genR();
			//noinspection unchecked
			el = new ArrayList[11];
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
					e.value.add(value);
					return;
				}
			}
			Set s = new Set();
			s.add(value);
			el[hash].add(new Entry(key, s));
			size++;
			if (size > /* 10 * */el.length) {
				resize();
			}
		}
		
		private void removeAll(String key) {
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
		
		private void removePair(String key, String value) {
			int hash = hash(key);
			if (el[hash] == null) {
				return;
			}
			Entry e;
			for (int i = 0; i < el[hash].size(); i++) {
				e = el[hash].get(i);
				if (e != null && e.key.equals(key)) {
					e.value.remove(value);
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
						for (int k = 0; k < temp[i].get(j).value.el.length; k++) {
							if (temp[i].get(j).value.el[k] != null) {
								for (int l = 0; l < temp[i].get(j).value.el[k].size(); l++) {
									add(temp[i].get(j).key, temp[i].get(j).value.el[k].get(l));
								}
							}
						}
					}
				}
			}
		}
		
		private String get(String key) {
			int hash = hash(key);
			if (el[hash] == null) {
				return "0";
			}
			Entry e;
			for (int i = 0; i < el[hash].size(); i++) {
				e = el[hash].get(i);
				if (e != null && e.key.equals(key)) {
					StringBuilder arr = new StringBuilder();
					for (List<String> bucket : e.value.el) {
						if (bucket != null)
						for (String string : bucket) {
							if (string != null) {
								arr.append(string).append(" ");
							}
						}
					}
					return e.value.size + " " + arr;
				}
			}
			return "0";
		}
		
		private void genR() {
			for (int i = 0; i < r.length; i++) {
				r[i] = random.nextInt(p);
			}
		}
		
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
//		    int hash = key.hashCode() % el.length;
			if (hash < 0) {
				hash += el.length;
			}
			return hash;
		}
		
		@Override
		public String toString() {
			return Arrays.toString(el);
		}
	}
	
	class Entry {
		String key;
		Set value;
		
		Entry(String key, Set value) {
			this.key = key;
			this.value = value;
		}
		
		@Override
		public String toString() {
			return key + ":" + value.toString();
		}
	}
	
	private static class Set {
		private int size;
		private List<String>[] el;
		private int p;
		private final Random random;
		private final int[] r = new int[20];
		
		private Set() {
			random = new Random(500);
			p = 11;
			genR();
			//noinspection unchecked
			el = new ArrayList[11];
		}
		
		private void add(String key) {
			int hash = hash(key);
			if (el[hash] == null) {
				el[hash] = new ArrayList<>();
			}
			if (!el[hash].contains(key)) {
				el[hash].add(key);
				size++;
			}
			if (size > el.length * 10) {
				resize();
			}
		}
		
		private void remove(String key) {
			int hash = hash(key);
			if (el[hash] == null) {
				return;
			}
			int indexOf = el[hash].indexOf(key);
			if (indexOf != -1) {
				el[hash].remove(indexOf);
				size--;
			}
		}
		
		private void resize() {
			//noinspection unchecked
			List<String>[] temp = new ArrayList[el.length];
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
						add(temp[i].get(j));
					}
				}
			}
		}
		
		private boolean contains(String key) {
			int hash = hash(key);
			if (el[hash] == null) {
				return false;
			}
			return el[hash].contains(key);
		}
		
		private void genR() {
			for (int i = 0; i < r.length; i++) {
				r[i] = random.nextInt(p);
			}
		}
		
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
//		    int hash = key.hashCode() % el.length;
			if (hash < 0) {
				hash += el.length;
			}
			return hash;
		}
		
		@Override
		public String toString() {
			return Arrays.toString(el);
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
		
	}
	
}

