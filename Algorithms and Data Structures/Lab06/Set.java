import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class Set {
	private int size;
	private List<String>[] el;
	private int p;
	private Random random;
	private int[] r;
	
	private Set(int maxLength) {
		random = new Random(500);
		p = 11;
		r = new int[maxLength];
		genR();
		//noinspection unchecked
		el = new ArrayList[11];
	}
	
	public static void main(String[] args) {
		Set set = new Set(10);
		set.add("1");
		set.add("2");
		set.add("3");
		set.add("4");
		System.out.println(set.toString());
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
//		int hash = key.hashCode() % el.length;
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