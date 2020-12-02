package queue;

public class ArrayDeque {
	
	//Inv: n >= 0 && (for i in 0...n-1: a[i] != null)
	private Object[] elements = new Object[10];
	private int size;
	private int head;
	private int tail;
	
	// el != null
	public void enqueue(Object el) {
		assert el != null;
		size++;
		ensureCapacity(size);
		elements[tail] = el;
		tail = (tail + 1) % elements.length;
	}
	// n' == n + 1 && (for i in 0...n-1: a'[i] == a[i]) && a'[n] == el
	
	// n > 0
	public Object dequeue() {
		assert size > 0;
		Object el = elements[head];
		elements[head] = null;
		head = (head + 1) % elements.length;
		size--;
		return el;
	}
	// n' == n-1 && R == a[0] && (for i in 1...n-1 a'[i - 1] == a[i])
	
	// n > 0
	public Object element() {
		assert size > 0;
		return elements[head];
	}
	// Immutable && R == a[0]
	
	// el != null
	public void push(Object el) {
		assert el != null;
		size++;
		ensureCapacity(size);
		head = (elements.length + head - 1) % elements.length;
		elements[head] = el;
	}
	// n' == n + 1 && (for i in 1...n: a'[i] == a[i]) && a'[0] == el
	
	// n > 0
	public Object remove() {
		assert size > 0;
		tail = (elements.length + tail - 1) % elements.length;
		Object el = elements[tail];
		elements[tail] = null;
		size--;
		return el;
	}
	// n' == n-1 && R == a[n-2] && (for i in 0...n-2 a'[i - 1] == a[i])
	
	// n > 0
	public Object peek() {
		assert size > 0;
		return elements[(elements.length + tail - 1) % elements.length];
	}
	// Immutable && R == a[n-1]
	
	// size > 0
	private void ensureCapacity(int size) {
		if (size >= elements.length) {
			Object[] temp = new Object[2 * size];
			int index = 0;
			for (int i = head; i != tail; i = (i + 1) % elements.length) {
				temp[index++] = elements[i];
			}
			elements = temp;
			head = 0;
			tail = index;
		}
	}
	// Immutable && size < elements.size
	
	//
	public int size() {
		return size;
	}
	// Immutable && R = n
	
	//
	public boolean isEmpty() {
		return size == 0;
	}
	// Immutable && (n == 0 && R = true) || (n != 0 && R == false)
	
	//
	public void clear() {
		elements = new Object[10];
		size = 0;
		head = 0;
		tail = 0;
	}
	// n == 0
	
}
