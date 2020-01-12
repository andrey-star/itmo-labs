import java.io.*;
import java.util.*;

public class A_StackVector {
	private int capacity;
	private int[] elements;
	private int size;

	private A_StackVector() {
		capacity = 8;
		elements = new int[capacity];
		size = 0;
	}

	private void push(int i) {
		if (size >= capacity) {
			capacity *= 2;
			int[] temp = new int[capacity];
			System.arraycopy(elements, 0, temp, 0, size);
			elements = temp;
		}
		elements[size++] = i;
	}

	private void pop() {
		if (size == 0) {
			throw new IndexOutOfBoundsException();
		}
		elements[--size] = 0;
		if (size * 4 <= capacity && size > 8) {
			capacity /= 2;
			int[] temp = new int[capacity];
			System.arraycopy(elements, 0, temp, 0, size);
			elements = temp;
		}
	}

	private int peek() {
		if (size == 0) {
			throw new IndexOutOfBoundsException();
		}
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

	public static void main(String[] args) throws IOException {
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("stack1.in")));
		PrintWriter out = new PrintWriter(new File("stack1.out"));
		A_StackVector sv = new A_StackVector();
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
