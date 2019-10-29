package queue;

public class ArrayDequeTest {
	
	private static void fill5(ArrayDeque queue) {
		for (int i = 0; i < 5; i++) {
			queue.enqueue(i + 1);
		}
	}
	
	public static void main(String[] args) {
		ArrayDeque deque = new ArrayDeque();
		fill5(deque);
		assert !deque.isEmpty();
		assert deque.size() == 5;
		assert deque.element().equals(1);
		Object dec = deque.dequeue();
		assert dec.equals(1);
		assert deque.element().equals(2);
		assert deque.size() == 4;
		dec = deque.dequeue();
		assert dec.equals(2);
		assert deque.element().equals(3);
		assert deque.size() == 3;
		dec = deque.dequeue();
		assert dec.equals(3);
		assert deque.element().equals(4);
		assert deque.size() == 2;
		dec = deque.dequeue();
		assert dec.equals(4);
		assert deque.element().equals(5);
		assert deque.size() == 1;
		
		fill5(deque);
		assert !deque.isEmpty();
		assert deque.size() == 6;
		assert deque.element().equals(5);
		dec = deque.dequeue();
		assert dec.equals(5);
		assert !deque.isEmpty();
		assert deque.size() == 5;
		assert deque.element().equals(1);
		dec = deque.dequeue();
		assert dec.equals(1);
		assert deque.element().equals(2);
		assert deque.size() == 4;
		dec = deque.dequeue();
		assert dec.equals(2);
		assert deque.element().equals(3);
		assert deque.size() == 3;
		dec = deque.dequeue();
		assert dec.equals(3);
		assert deque.element().equals(4);
		assert deque.size() == 2;
		dec = deque.dequeue();
		assert dec.equals(4);
		assert deque.element().equals(5);
		assert deque.size() == 1;
		dec = deque.dequeue();
		assert dec.equals(5);
		assert deque.size() == 0;
		assert deque.isEmpty();
		fill5(deque);
		deque.clear();
		assert deque.isEmpty();
		
		deque.enqueue(1);
		assert deque.element().equals(1);
		assert deque.size() == 1;
		deque.push(2);
		assert deque.size() == 2;
		assert deque.element().equals(2);
		assert deque.peek().equals(1);
		Object rem = deque.remove();
		assert rem.equals(1);
		assert deque.peek().equals(2);
		assert deque.element().equals(2);
		assert deque.size() == 1;
		deque.remove();
		assert deque.size() == 0;
		assert deque.isEmpty();
		
	}
	
}
