import java.io.*;
import java.util.Arrays;

public class G_QueueMin {
	private StackVector toPush;
	private StackVector toPop;

	private G_QueueMin() {
		toPush = new StackVector();
		toPop = new StackVector();
	}

	private void push(int i) {
		toPush.push(i);
	}

	private Pair pop() {
		if (toPop.size == 0) {
			int unIndexed = toPush.size;
			for (int i = 0; i < unIndexed; i++) {
				toPop.push(toPush.pop().item);
			}
		}
		return toPop.pop();
	}

	private int min() {
		if (toPop.size == 0) {
			return toPush.peek().min;
		}
		return Math.min(toPop.peek().min, toPush.peek().min);
	}


	private static class Pair {
		int min;
		int item;

		public Pair(int item, int min) {
			this.item = item;
			this.min = min;
		}

		@Override
		public String toString() {
			return item + " " + min;
		}
	}

	private static class StackVector {
		private int capacity;
		private Pair[] elements;
		private int size;

		private StackVector() {
			capacity = 8;
			elements = new Pair[capacity];
			size = 0;
		}

		private void push(int i) {
			if (size >= capacity) {
				capacity *= 2;
				Pair[] temp = new Pair[capacity];
				System.arraycopy(elements, 0, temp, 0, size);
				elements = temp;
			}

			if (size == 0) {
				elements[size++] = new Pair(i, i);
			} else {
				elements[size] = new Pair(i, Math.min(i, elements[size - 1].min));
				size++;
			}
		}

		private Pair pop() {
			Pair last = elements[size - 1];
			elements[--size] = null;
			if (size * 4 <= capacity && size > 8) {
				capacity /= 2;
				Pair[] temp = new Pair[capacity];
				System.arraycopy(elements, 0, temp, 0, size);
				elements = temp;
			}
			return last;
		}

		private Pair peek() {
			return elements[size - 1];
		}

		@Override
		public String toString() {
			return "StackVector{" +
					"capacity=" + capacity +
					", elements=" + Arrays.toString(elements) +
					", size=" + size +
					'}';
		}
	}

	@Override
	public String toString() {
		return "QueueMin{" +
				"toPush=" + toPush.toString() +
				", toPop=" + toPop.toString() +
				'}';
	}

	public static void main(String[] args) throws IOException {
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("queuemin2.in")));
		PrintWriter out = new PrintWriter(new File("queuemin2.out"));
		G_QueueMin sv = new G_QueueMin();
		String[] first = reader.readLine().split(" ");
		int n = Integer.parseInt(first[0]);
		int m = Integer.parseInt(first[1]);
		int k = Integer.parseInt(first[2]);
		int[] arr = new int[n];
		String[] second = reader.readLine().split(" ");
		int a = Integer.parseInt(second[0]);
		int b = Integer.parseInt(second[1]);
		int c = Integer.parseInt(second[2]);
		String[] third = reader.readLine().split(" ");
		for (int i = 0; i < k; i++) {
			arr[i] = Integer.parseInt(third[i]);
		}
		for (int i = k; i < n; i++) {
			arr[i] = a * arr[i - 2] + b * arr[i - 1] + c;
		}
		for (int i = 0; i < m; i++) {
			sv.push(arr[i]);
		}
		long sum = 0;
		for (int i = 0; i < n - m + 1; i++) {
			sum += sv.min();
			System.out.println(sv.min());
			sv.pop();
			if (i != n - m) {
				sv.push(arr[i + m]);
			}
		}
		out.println(sum);
		reader.close();
		out.close();
	}
}