package search;

import java.util.Arrays;

public class BinarySearch {
	
	// Pre: a - sorted in non-increment order && ∃ i : a[i] <= key && r' - l' >= 1 &&
	//      && (l' == -1 || (0 <= l' < a.size && a[l'] > key))
	//      && (r' == a.size || (0 <= r' < a.size && a[r'] <= key))
	private static int binRec(int[] a, int key, int l, int r) {
		if (r - l <= 1) {
			// Pre && r' - l' >= 1
			// r' - l' >= 1 && r' - l' <= 1 -> l' = r' + 1
			// ∃ i : a[i] <= key -> l' < a.size - 1 -> r' < a.size
			// Pre && r' < a.size -> (0 <= r' < a.size && a[r'] <= key)
			// r' == 0
			// Pre && 0 < r' < a.size -> Pre && 0 <= l' -> a[l'] > key -> a[r' - 1] > key
			return r;
		}
		
		// Pre && r' - l' > 1
		int m = (l + r) / 2;
		// m = (l' + r')/2 > (l' + l' + 1)/2 == (2l' + 1)/2 == l' -> m > l'
		// m = (l' + r')/2 < (r' + r' - 1)/2 == (2r' + 1)/2 == r' -> m < r'
		// l' < m < r'
		
		
		if (a[m] > key) {
			// Pre && r' - l' > 1 && m == (l' + r')/2 && l' < m < r' && a[m] > key
			// l_old := l'
			// r_old := r'
			l = m;
			// l' == m && r' == r_old && m < r_old -> l' < r' -> r' - l' >= 1
			// r' == r_old -> (r' == a.size || (0 <= r' < a.size && a[r'] <= key))
			// Pre && l_old < m < r_old && l' == m && a[m] > key ->
			//      -> (0 <= l' < a.size && a[l'] > key)
			
			// Pre'
			return binRec(a, key, l, r);
		} else {
			// Pre && l' + 1 < r' && m == (l' + r')/2 && l' < m < r' && a[m] <= key
			// l_old := l'
			// r_old := r'
			r = m;
			// r' == m && l' == l_old && l_old < m -> l' < r' -> r' - l' >= 1
			// l' == l_old -> (l' == -1 || (0 <= l' < a.size && a[l'] > key))
			// Pre && l_old < m < r_old && r' == m && a[m] <= key ->
			//      -> (0 <= r' < a.size && a[r'] <= key)
			
			// Pre'
			return binRec(a, key, l, r);
		}
	}
	// Post: (0 <= R < a.size && a[R] <= key && (R == 0 || a[R - 1] > key)
	
	
	// Pre: a - sorted in non-increment order && ∃ i : a[i] <= key
	private static int binIter(int[] a, int key) {
		// Pre
		int l = -1;
		int r = a.length;
		
		// Inv: r' - l' >= 1 && (l' == -1 || (0 <= l' < a.size && a[l'] > key))
		//      && (r' == a.size || (0 <= r' < a.size && a[r'] <= key))
		// Pre && Inv
		while (r - l > 1) {
			// Pre && Inv && r' - l' > 1
			int m = (l + r) / 2;
			// m = (l' + r')/2 > (l' + l' + 1)/2 == (2l' + 1)/2 == l' -> m > l'
			// m = (l' + r')/2 < (r' + r' - 1)/2 == (2r' + 1)/2 == r' -> m < r'
			// l' < m < r'
			
			// Pre && Inv && r' - l' > 1 && m == (l' + r')/2 && l' < m < r'
			if (a[m] > key) {
				// Pre && Inv && r' - l' > 1 && m == (l' + r')/2 && l' < m < r' && a[m] > key
				// l_old := l'
				// r_old := r'
				l = m;
				// l' == m && r' == r_old && m < r_old -> l' < r' -> r' - l' >= 1
				// r' == r_old -> (r' == a.size || (0 <= r' < a.size && a[r'] <= key))
				// Pre && l_old < m < r_old && l' == m && a[m] > key ->
				//      -> (0 <= l' < a.size && a[l'] > key)
				
				// Pre && Inv
			} else {
				// Pre && Inv && l' + 1 < r' && m == (l' + r')/2 && l' < m < r' && a[m] <= key
				// l_old := l'
				// r_old := r'
				r = m;
				// r' == m && l' == l_old && l_old < m -> l' < r' -> r' - l' >= 1
				// l' == l_old -> (l' == -1 || (0 <= l' < a.size && a[l'] > key))
				// Pre && l_old < m < r_old && r' == m && a[m] <= key ->
				//      -> (0 <= r' < a.size && a[r'] <= key)
				
				// Pre && Inv
			}
			// l_old < m < r_old -> (l_old < l' || r' < r_old) -> r' - l' < r_old - l_old ->
			//      -> r' - l' segment has become shorter by a minimum of 1, which means r' - l' <= 1 is reachable
			//      therefore while loop will exit.
		}
		// Pre && Inv && r' - l' >= 1
		// r' - l' >= 1 && r' - l' <= 1 -> l' = r' + 1
		// ∃ i : a[i] <= key -> l' < a.size - 1 -> r' < a.size
		// Inv && r' < a.size -> (0 <= r' < a.size && a[r'] <= key)
		// r' == 0
		// Inv && 0 < r' < a.size -> Inv && 0 <= l' -> a[l'] > key
		return r;
	}
	// Post: (0 <= R < a.size && a[R] <= key && (R == 0 || a[R - 1] > key)
	
	
	public static void main(String[] args) {
		int x = Integer.parseInt(args[0]);
		int[] a = Arrays.stream(args).skip(1).mapToInt(Integer::parseInt).toArray();
		System.out.println(binIter(a, x));
		System.out.println(binRec(a, x, -1, a.length));
	}
	
}
