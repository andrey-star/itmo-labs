package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public class Main {
	public static void main(String[] args) {
		Predicate<Integer> pre = new Predicate<Integer>() {
			@Override
			public boolean test(Integer o) {
				return o > 5;
			}
		};
		
		Function<Integer, Integer> func = new Function<Integer, Integer>() {
			@Override
			public Integer apply(Integer integer) {
				return integer + 1;
			}
		};
		
		Queue q = new ArrayQueue();
		q.size();
	}
}
