package queue;

public class ArrayDequeADT {
	
	// n >= 0 && (for i in 0...n-1: a[i] != null)
	private int size;
	private Object[] elements = new Object[10];
	private int head;
	private int tail;
	
	// el != null
	public static void enqueue(ArrayDequeADT queue, Object el) {
		assert el != null;
		queue.size++;
		ensureCapacity(queue, queue.size);
		queue.elements[queue.tail] = el;
		queue.tail = (queue.tail + 1) % queue.elements.length;
	}
	// n' == n + 1 && (for i in 0...n-1: a'[i] == a[i]) && a'[n] == el
	
	// n > 0
	public static Object dequeue(ArrayDequeADT queue) {
		assert queue.size > 0;
		Object el = queue.elements[queue.head];
		queue.elements[queue.head] = null;
		queue.head = (queue.head + 1) % queue.elements.length;
		queue.size--;
		return el;
	}
	// n' == n-1 && R == a[0] && (for i in 1...n-1 a'[i - 1] == a[i])
	
	// n > 0
	public static Object element(ArrayDequeADT queue) {
		assert queue.size > 0;
		return queue.elements[queue.head];
	}
	// Immutable && R == a[0]
	
	// el != null
	public static void push(ArrayDequeADT queue, Object el) {
		assert el != null;
		queue.size++;
		ensureCapacity(queue, queue.size);
		queue.head = (queue.elements.length + queue.head - 1) % queue.elements.length;
		queue.elements[queue.head] = el;
	}
	// n' == n + 1 && (for i in 1...n: a'[i] == a[i]) && a'[0] == el
	
	// n > 0
	public static Object remove(ArrayDequeADT queue) {
		assert queue.size > 0;
		queue.tail = (queue.elements.length + queue.tail - 1) % queue.elements.length;
		Object el = queue.elements[queue.tail];
		queue.elements[queue.tail] = null;
		queue.size--;
		return el;
	}
	// n' == n-1 && R == a[n-2] && (for i in 0...n-2 a'[i - 1] == a[i])
	
	// n > 0
	public static Object peek(ArrayDequeADT queue) {
		assert queue.size > 0;
		return queue.elements[(queue.elements.length + queue.tail - 1) % queue.elements.length];
	}
	// Immutable && R == a[n-1]
	
	// size > 0
	private static void ensureCapacity(ArrayDequeADT queue, int size) {
		if (size >= queue.elements.length) {
			Object[] temp = new Object[2 * size];
			int index = 0;
			for (int i = queue.head; i != queue.tail; i = (i + 1) % queue.elements.length) {
				temp[index++] = queue.elements[i];
			}
			queue.elements = temp;
			queue.head = 0;
			queue.tail = index;
		}
	}
	// Immutable && size < elements.size
	
	//
	public static int size(ArrayDequeADT queue) {
		return queue.size;
	}
	// Immutable && R = n
	
	//
	public static boolean isEmpty(ArrayDequeADT queue) {
		return queue.size == 0;
	}
	// Immutable && (n == 0 && R = true) || (n != 0 && R == false)
	
	//
	public static void clear(ArrayDequeADT queue) {
		queue.elements = new Object[10];
		queue.size = 0;
		queue.head = 0;
		queue.tail = 0;
	}
	// n == 0
}