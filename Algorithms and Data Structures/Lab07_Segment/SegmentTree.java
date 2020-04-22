import java.util.function.BiFunction;

public class SegmentTree {
	
	private final BiFunction<Integer, Integer, Integer> f;
	private final Entry[] tree;
	private final int neutral;
	
	public SegmentTree(int[] a, int neutral, BiFunction<Integer, Integer, Integer> f) {
		this.f = f;
		this.neutral = neutral;
		int n = 1 << (31 - Integer.numberOfLeadingZeros(a.length));
		if (Integer.bitCount(a.length) != 1) {
			n <<= 1;
		}
		int[] aLong = new int[n];
		for (int i = 0; i < n; i++) {
			if (i < a.length) {
				aLong[i] = a[i];
			} else {
				aLong[i] = neutral;
			}
		}
		tree = new Entry[2 * n - 1];
		for (int i = 0; i < n; i++) {
			tree[n - 1 + i] = new Entry(aLong[i]);
		}
		for (int i = n - 2; i >= 0; i--) {
			tree[i] = new Entry(f.apply(tree[left(i)].x, tree[right(i)].x));
		}
	}
	
	private int query(int node, int a, int b, int l, int r) {
		if (l >= b || r <= a) {
			return neutral;
		}
		if (l >= a && r <= b) {
			return tree[node].x;
		}
		int m = (l + r) / 2;
		return f.apply(query(left(node), a, b, l, m), query(right(node), a, b, m, r));
	}
	
	/**
	 *
	 * @param a left bound (inclusive)
	 * @param b left bound (exclusive)
	 */
	private int query(int a, int b) {
		return query(0, a, b, 0, (tree.length + 1) / 2);
	}
	
	private void set(int i, int v) {
		int n = (tree.length + 1) / 2;
		i = n - 1 + i;
		tree[i] = new Entry(v);
		if (i != 0) {
			update(parent(i));
		}
	}
	
	private void update(int i) {
		tree[i] = new Entry(f.apply(tree[left(i)].x, tree[right(i)].x));
		if (i == 0) {
			return;
		}
		update(parent(i));
	}
	
	private int left(int i) {
		return 2 * i + 1;
	}
	
	private int right(int i) {
		return left(i) + 1;
	}
	
	private int parent(int i) {
		return (i - 1) / 2;
	}
	
	private static class Entry {
		
		int x;
		
		Entry(int x) {
			this.x = x;
		}
	}
	
	public static void main(String[] args) {
		SegmentTree tree = new SegmentTree(new int[]{1, 2 ,3, 4, 5}, Integer.MAX_VALUE, Integer::min);
		System.out.println(tree.query(0, 5));
	}
}
