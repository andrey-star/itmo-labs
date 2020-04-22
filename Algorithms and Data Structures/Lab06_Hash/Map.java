import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class Map {
	
	private int size;
	private List<Entry>[] el;
	private int p;
	private final int[] r;
	private final Random random;
	
	private Map(int maxLength) {
		random = new Random(500);
		p = 11;
		r = new int[maxLength];
		genR();
		//noinspection unchecked
		el = new ArrayList[11];
	}
	
	public static void main(String[] args) {
		Map map = new Map(10);
		map.add("1", "2");
		map.add("3", "2");
		map.add("4", "2");
		System.out.println(map.toString());
		map.remove("1");
		System.out.println(map.toString());
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
		if (size > /* 10 * */el.length) {
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
			return null;
		}
		Entry e;
		for (int i = 0; i < el[hash].size(); i++) {
			e = el[hash].get(i);
			if (e != null && e.key.equals(key)) {
				return e.value;
			}
		}
		return null;
	}
	
	private void genR() {
		for (int i = 0; i < r.length; i++) {
			r[i] = random.nextInt(p);
		}
	}
	
	// polynomial hash
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
		if (hash < 0) {
			hash += el.length;
		}
		return hash;
	}
	
	@Override
	public String toString() {
		return Arrays.toString(el);
	}
	
	static class Entry {
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
}
	
	