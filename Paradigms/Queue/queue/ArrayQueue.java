package queue;

import java.util.Arrays;

public class ArrayQueue extends AbstractQueue {
	private Object[] elements = new Object[10];
	private int head;
	
	@Override
	public void enqueueImpl(Object el) {
		ensureCapacity(size + 1);
		elements[(head + size) % elements.length] = el;
	}
	
	@Override
	public void dequeueImpl() {
		elements[head] = null;
		head = (head + 1) % elements.length;
	}
	
	@Override
	public Object elementImpl() {
		return elements[head];
	}
	
	private void ensureCapacity(int size) {
		if (size >= elements.length) {
			Object[] temp = new Object[2 * size];
			int index = 0;
			int tail = (head + this.size) % elements.length;
			for (int i = head; i != tail; i = (i + 1) % elements.length) {
				temp[index++] = elements[i];
			}
			elements = temp;
			head = 0;
		}
	}
	
	@Override
	public void clearImpl() {
		elements = new Object[10];
		head = 0;
	}
	
	@Override
	public ArrayQueue makeCopy() {
		ArrayQueue queue = new ArrayQueue();
		queue.size = size;
		queue.head = head;
		queue.elements = Arrays.copyOf(elements, elements.length);
		return queue;
	}
	
}