import java.io.*;
import java.util.Arrays;
import java.util.Scanner;

public class F_PostfixNotation {

	private static class StackVector {

		private int capacity;
		private int[] elements;
		private int size;

		private StackVector() {
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
	}


	public static void main(String[] args) throws IOException {
		StackVector sl = new StackVector();
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("postfix.in")));
		PrintWriter out = new PrintWriter(new File("postfix.out"));
		String[] op = reader.readLine().split(" ");
		reader.close();
		for (String cmd : op) {
			if (cmd.equals("+") || cmd.equals("-") || cmd.equals("*")) {
				int a = sl.peek();
				sl.pop();
				int b = sl.peek();
				sl.pop();
				if (cmd.equals("+")) {
					sl.push(a + b);
				} else if (cmd.equals("-")) {
					sl.push(b - a);
				} else {
					sl.push(a * b);
				}
			} else {
				sl.push(Integer.parseInt(cmd));
			}
		}
		out.println(sl.peek());
		out.close();
	}
}
