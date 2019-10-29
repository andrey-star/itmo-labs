package search;

import java.util.Arrays;

public class BinarySearchSpan {
	
	// Pre: a - sorted in non-increment order && r - l >= 1 &&
	//      && (l == -1 || (0 <= l < a.size && a[l] >= key))
	//      && (r == a.size || (0 <= r < a.size && a[r] < key))
	private static int binRec(int[] a, int key, int l, int r) {
		if (r - l <= 1) {
			// Pre && r' - l' >= 1
			// r' - l' >= 1 && r' - l' <= 1 -> l' = r' + 1
			// Pre && r' < a.size -> (0 <= r' < a.size && a[r'] < key)
			// -1 <= l' < a.size && l' = r' + 1 -> 0 <= r' <= a.size
			// Pre && r' == a.size -> l' == a.last -> a.last >= key -> all els >= key
			// Pre && 0 <= r' < a.size -> a[r'] < key
			// Pre && r' == 0 -> a.first < key -> all els < key
			// Pre && 0 < r' <= a.size -> 0 <= l' < a.size -> a[l'] >= key
			return r;
		}
		// Pre && r' - l' > 1
		int m = (l + r) / 2;
		// m = (l' + r')/2 > (l' + l' + 1)/2 == (2l' + 1)/2 == l' -> m > l'
		// m = (l' + r')/2 < (r' + r' - 1)/2 == (2r' + 1)/2 == r' -> m < r'
		// l' < m < r'
		
		
		if (a[m] >= key) {
			// Pre && r' - l' > 1 && m == (l' + r')/2 && l' < m < r' && a[m] >= key
			// l_old := l'
			// r_old := r'
			l = m;
			// l' == m && r' == r_old && m < r_old -> l' < r' -> r' - l' >= 1
			// r' == r_old -> (r' == a.size || (0 <= r' < a.size && a[r'] < key))
			// Pre && l_old < m < r_old && l' == m && a[m] >= key ->
			//      -> (0 <= l' < a.size && a[l'] >= key)
			
			// Pre'
			return binRec(a, key, l, r);
		} else {
			// Pre && l' + 1 < r' && m == (l' + r')/2 && l' < m < r' && a[m] < key
			// l_old := l'
			// r_old := r'
			r = m;
			// r' == m && l' == l_old && l_old < m -> l' < r' -> r' - l' >= 1
			// l' == l_old -> (l' == -1 || (0 <= l' < a.size && a[l'] >= key))
			// Pre && l_old < m < r_old && r' == m && a[m] < key ->
			//      -> (0 <= r' < a.size && a[r'] < key)
			
			// Pre'
			return binRec(a, key, l, r);
		}
	}
	// Post: 0 <= R <= a.size && ((R == a.size && all els >= key) || a[R] < key) &&
	//      && ((R == 0 && all els < key) || a[R - 1] >= key)
	
	
	// Pre: a - sorted in non-increment order
	private static int binIter(int[] a, int key) {
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
		// -1 <= l' < a.size && l' = r' + 1 -> 0 <= r' <= a.size
		// r' == a.size -> l' == a.last -> a.last > key -> all els > key
		// Pre && 0 <= r' < a.size -> a[r'] <= key
		// r' == 0 -> a.first <= key -> all els <= key
		// Pre && 0 < r' <= a.size -> 0 <= l' < a.size -> l' == r' - 1 && a[l'] > key -> a[r' - 1] > key
		return r;
	}
	// Post: 0 <= R <= a.size && ((R == a.size && all els > key) || a[R] <= key) &&
	//      && ((R == 0 && all els <= key) || a[R - 1] > key)
	
	
	public static void main(String[] args) {
		int x = Integer.parseInt(args[0]);
		int[] a = Arrays.stream(args).skip(1).mapToInt(Integer::parseInt).toArray();
		int l = binIter(a, x);
		int r = binRec(a, x, -1, a.length);
		// l == a.size -> all els > key -> r == a.size -> bound == a.size && length == 0 -> correct
		// r == 0 -> all els < key -> l == 0 -> l == 0 && length == 0 -> correct
		// l == 0 && 0 < r <= a.size -> all els <= key -> bound = 0 && ((a[r - 1] >= key && a[r] < key) || all els >= key) ->
		//      -> bound = 0 && length = r - l -> correct
		// r == a.size && 0 <= l < a.size -> all els >= key -> bound == l && ((a[l - 1] > key && a[l] <= key) || all els > key) ->
		//      -> bound = 0 && length = r - l -> correct
		// 0 < l < a.size && 0 < r < a.size -> a[l] <= key && a[l - 1] > key && a[r - 1] >= key && a[r] < key ->
		//      -> bound == l && length = r - l -> correct
		System.out.println(l + " " + (r - l));
		
	}
}
