package queue;

public class ArrayDequeADTTest {
	
	private static void fill5(ArrayDequeADT queue) {
		for (int i = 0; i < 5; i++) {
			ArrayDequeADT.enqueue(queue,i + 1);
		}
	}
	
	public static void main(String[] args) {
		ArrayDequeADT queue = new ArrayDequeADT();
		fill5(queue);
		assert !ArrayDequeADT.isEmpty(queue);
		assert ArrayDequeADT.size(queue) == 5;
		assert ArrayDequeADT.element(queue).equals(1);
		Object dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(1);
		assert ArrayDequeADT.element(queue).equals(2);
		assert ArrayDequeADT.size(queue) == 4;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(2);
		assert ArrayDequeADT.element(queue).equals(3);
		assert ArrayDequeADT.size(queue) == 3;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(3);
		assert ArrayDequeADT.element(queue).equals(4);
		assert ArrayDequeADT.size(queue) == 2;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(4);
		assert ArrayDequeADT.element(queue).equals(5);
		assert ArrayDequeADT.size(queue) == 1;
		
		fill5(queue);
		assert !ArrayDequeADT.isEmpty(queue);
		assert ArrayDequeADT.size(queue) == 6;
		assert ArrayDequeADT.element(queue).equals(5);
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(5);
		assert !ArrayDequeADT.isEmpty(queue);
		assert ArrayDequeADT.size(queue) == 5;
		assert ArrayDequeADT.element(queue).equals(1);
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(1);
		assert ArrayDequeADT.element(queue).equals(2);
		assert ArrayDequeADT.size(queue) == 4;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(2);
		assert ArrayDequeADT.element(queue).equals(3);
		assert ArrayDequeADT.size(queue) == 3;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(3);
		assert ArrayDequeADT.element(queue).equals(4);
		assert ArrayDequeADT.size(queue) == 2;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(4);
		assert ArrayDequeADT.element(queue).equals(5);
		assert ArrayDequeADT.size(queue) == 1;
		dec = ArrayDequeADT.dequeue(queue);
		assert dec.equals(5);
		assert ArrayDequeADT.size(queue) == 0;
		assert ArrayDequeADT.isEmpty(queue);
		fill5(queue);
		ArrayDequeADT.clear(queue);
		assert ArrayDequeADT.isEmpty(queue);
		
		ArrayDequeADT.enqueue(queue, 1);
		assert ArrayDequeADT.element(queue).equals(1);
		assert ArrayDequeADT.size(queue) == 1;
		ArrayDequeADT.push(queue,2);
		assert ArrayDequeADT.size(queue) == 2;
		assert ArrayDequeADT.element(queue).equals(2);
		assert ArrayDequeADT.peek(queue).equals(1);
		Object rem = ArrayDequeADT.remove(queue);
		assert rem.equals(1);
		assert ArrayDequeADT.peek(queue).equals(2);
		assert ArrayDequeADT.element(queue).equals(2);
		assert ArrayDequeADT.size(queue) == 1;
		ArrayDequeADT.remove(queue);
		assert ArrayDequeADT.size(queue) == 0;
		assert ArrayDequeADT.isEmpty(queue);
	}
	
}
