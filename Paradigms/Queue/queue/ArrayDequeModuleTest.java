package queue;

public class ArrayDequeModuleTest {
	
	private static void fill5() {
		for (int i = 0; i < 5; i++) {
			ArrayDequeModule.enqueue(i + 1);
		}
	}
	
	public static void main(String[] args) {
		fill5();
		assert !ArrayDequeModule.isEmpty();
		assert ArrayDequeModule.size() == 5;
		assert ArrayDequeModule.element().equals(1);
		Object dec = ArrayDequeModule.dequeue();
		assert dec.equals(1);
		assert ArrayDequeModule.element().equals(2);
		assert ArrayDequeModule.size() == 4;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(2);
		assert ArrayDequeModule.element().equals(3);
		assert ArrayDequeModule.size() == 3;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(3);
		assert ArrayDequeModule.element().equals(4);
		assert ArrayDequeModule.size() == 2;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(4);
		assert ArrayDequeModule.element().equals(5);
		assert ArrayDequeModule.size() == 1;
		
		fill5();
		assert !ArrayDequeModule.isEmpty();
		assert ArrayDequeModule.size() == 6;
		assert ArrayDequeModule.element().equals(5);
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(5);
		assert !ArrayDequeModule.isEmpty();
		assert ArrayDequeModule.size() == 5;
		assert ArrayDequeModule.element().equals(1);
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(1);
		assert ArrayDequeModule.element().equals(2);
		assert ArrayDequeModule.size() == 4;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(2);
		assert ArrayDequeModule.element().equals(3);
		assert ArrayDequeModule.size() == 3;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(3);
		assert ArrayDequeModule.element().equals(4);
		assert ArrayDequeModule.size() == 2;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(4);
		assert ArrayDequeModule.element().equals(5);
		assert ArrayDequeModule.size() == 1;
		dec = ArrayDequeModule.dequeue();
		assert dec.equals(5);
		assert ArrayDequeModule.size() == 0;
		assert ArrayDequeModule.isEmpty();
		fill5();
		ArrayDequeModule.clear();
		assert ArrayDequeModule.isEmpty();
		
		ArrayDequeModule.enqueue(1);
		assert ArrayDequeModule.element().equals(1);
		assert ArrayDequeModule.size() == 1;
		ArrayDequeModule.push(2);
		assert ArrayDequeModule.size() == 2;
		assert ArrayDequeModule.element().equals(2);
		assert ArrayDequeModule.peek().equals(1);
		Object rem = ArrayDequeModule.remove();
		assert rem.equals(1);
		assert ArrayDequeModule.peek().equals(2);
		assert ArrayDequeModule.element().equals(2);
		assert ArrayDequeModule.size() == 1;
		ArrayDequeModule.remove();
		assert ArrayDequeModule.size() == 0;
		assert ArrayDequeModule.isEmpty();
	}
	
}
