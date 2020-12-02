package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public interface Queue {
	
	// Inv: n >= 0 && (for i in 0...n-1: a[i] != null)
	
	// el != null
	void enqueue(Object el);
	// n' == n + 1 && (for i in 0...n-1: a'[i] == a[i]) && a'[n] == el
	
	// n > 0
	Object element();
	// Immutable && R == a[0]
	
	// n > 0
	Object dequeue();
	// n' == n-1 && R == a[0] && (for i in 1...n-1 a'[i - 1] == a[i])
	
	boolean isEmpty();
	// Immutable && (n == 0 && R = true) || (n != 0 && R == false)
	
	int size();
	// Immutable && R = n
	
	void clear();
	// n == 0
	
	Object[] toArray();
	// n' == n && for i in 0...n'-1: R[i] == a[i]
	
	// pred != null
	Queue filter(Predicate<Object> pred);
	// Immutable n' <= n && for i in 0...n'-1: a'[i] != null && pred(a'[i]) == true && preserves order
	
	// func != null
	Queue map(Function<Object, Object> func);
	// Immutable n' == n && for i in 0...n'-1: a'[i] != null && a'[i] == func(a[i]) && preserves order
}
