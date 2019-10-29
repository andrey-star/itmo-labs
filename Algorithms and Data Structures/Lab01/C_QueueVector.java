import java.io.*;
import java.util.Arrays;

public class C_QueueVector {
	private int capacity;
	private int[] elements;
	private int size;
	private int start;

	public C_QueueVector() {
		capacity = 8;
		elements = new int[capacity];
		size = 0;
		start = 0;
	}

	private void push(int i) {
		if (start + size >= capacity) {
			if (size*2 > capacity && capacity >= 8) {
				capacity *= 2;
			}
			int[] temp = new int[capacity];
			System.arraycopy(elements, start, temp, 0, size);
			elements = temp;
			start = 0;
		}
		elements[start + size++] = i;
	}

	private void pop() {
		if (size == 0) {
			throw new IndexOutOfBoundsException();
		}
		size--;
		elements[start++] = 0;
		if (size * 4 <= capacity && capacity > 8) {
			capacity /= 2;
			int[] temp = new int[capacity];
			System.arraycopy(elements, start, temp, 0, size);
			elements = temp;
			start = 0;
		}
	}

	private int peek() {
		if (size == 0) {
			throw new IndexOutOfBoundsException();
		}
		return elements[start];
	}


	@Override
	public String toString() {
		return "QueueVector{" +
				"capacity=" + capacity +
				", elements=" + Arrays.toString(elements) +
				", size=" + size +
				", start=" + start +
				'}';
	}

	public static void main(String[] args) throws IOException {
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("queue1.in")));
		PrintWriter out = new PrintWriter(new File("queue1.out"));
		C_QueueVector sv = new C_QueueVector();
		int n = Integer.parseInt(reader.readLine());
		for (int i = 0; i < n; i++) {
			String[] cmd = reader.readLine().split(" ");
			if (cmd[0].equals("+")) {
				sv.push(Integer.parseInt(cmd[1]));
			} else {
				out.println(sv.peek());
				sv.pop();
			}
		}
		reader.close();
		out.close();
	}
}
