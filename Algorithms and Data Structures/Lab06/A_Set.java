import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class A_Set {
	
	private int size;
	private List<Integer>[] el;
	private int a, b, p;
	private Random r;
	private FastScanner in;
	private PrintWriter out;
	
	private A_Set() {
		r = new Random(500);
		a = r.nextInt(11);
		b = r.nextInt(11);
		p = 11;
		//noinspection unchecked
		el = new ArrayList[11];
	}
	
	public static void main(String[] args) {
		new A_Set().run();
	}
	
	private void solve() {
		A_Set s = new A_Set();
		String cmd;
		int val;
		while (true) {
			try {
				cmd = in.next();
			} catch (NullPointerException e) {
				break;
			}
			val = in.nextInt();
			if (cmd.equals("insert")) {
				s.add(val);
			} else if (cmd.equals("delete")) {
				s.remove(val);
			} else {
				out.println(s.contains(val));
			}
		}
	}
	
	private void add(int key) {
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
	
	private void remove(int key) {
		StringBuilder s = new StringBuilder();
		for (int i = 0; i < 5; i++) {
			s.insert(0, '1');
		}
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
		List<Integer>[] temp = new ArrayList[el.length];
		System.arraycopy(el, 0, temp, 0, el.length);
		p = BigInteger.valueOf(2 * el.length).nextProbablePrime().intValue();
		//noinspection unchecked
		el = new ArrayList[2 * el.length];
		size = 0;
		a = r.nextInt(p);
		b = r.nextInt(p);
		//noinspection ForLoopReplaceableByForEach
		for (int i = 0; i < temp.length; i++) {
			if (temp[i] != null) {
				for (int j = 0; j < temp[i].size(); j++) {
					add(temp[i].get(j));
				}
			}
		}
	}
	
	private boolean contains(int key) {
		int hash = hash(key);
		if (el[hash] == null) {
			return false;
		}
		return el[hash].contains(key);
	}
	
	private int hash(int key) {
		int hash = (int) (((long) a * key + b) % p) % el.length;
		if (hash < 0) {
			hash += el.length;
		}
		return hash;
	}
	
	private void run() {
		long start = System.currentTimeMillis();
		try {
			in = new FastScanner(new File("set.in"));
			out = new PrintWriter(new File("set.out"));
			solve();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		int mb = 1024 * 1024;
		Runtime runtime = Runtime.getRuntime();
		System.out.println(runtime.totalMemory() / mb);
		System.out.println("Used Memory: " + (runtime.totalMemory() - runtime.freeMemory()) / mb + " mb");
		System.out.println("Total: " + (System.currentTimeMillis() - start) / 1000.0 + " s");
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

