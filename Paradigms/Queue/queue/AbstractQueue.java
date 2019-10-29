package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
	
	protected int size;
	
	@Override
	public void enqueue(Object o) {
		assert o != null;
		enqueueImpl(o);
		size++;
	}
	
	protected abstract void enqueueImpl(Object o);
	
	@Override
	public Object dequeue() {
		Object o = element();
		dequeueImpl();
		size--;
		return o;
	}
	
	protected abstract void dequeueImpl();
	
	@Override
	public Object element() {
		assert size > 0;
		return elementImpl();
	}
	
	protected abstract Object elementImpl();
	
	public Object[] toArray() {
		Object[] array = new Object[size];
		for (int i = 0; i < size; i++) {
			array[i] = dequeue();
			enqueue(array[i]);
		}
		return array;
	}
	
	protected abstract Queue makeCopy();

	@Override
	public Queue filter(Predicate<Object> pred) {
		assert pred != null;
		Queue queue = makeCopy();
		for (int i = 0; i < size; i++) {
			Object temp = queue.dequeue();
			if (pred.test(temp)) {
				queue.enqueue(temp);
			}
		}
		return queue;
	}
	
	@Override
	public Queue map(Function<Object, Object> func) {
		assert func != null;
		Queue queue = makeCopy();
		for (int i = 0; i < size; i++) {
			Object temp = queue.dequeue();
			queue.enqueue(func.apply(temp));
		}
		return queue;
	}
	
	@Override
	public int size() {
		return size;
	}
	
	@Override
	public boolean isEmpty() {
		return size == 0;
	}
	
	@Override
	public void clear() {
		size = 0;
		clearImpl();
	}
	
	
	protected abstract void clearImpl();
	
}
