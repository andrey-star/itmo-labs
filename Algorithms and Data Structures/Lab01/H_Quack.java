import java.io.*;
import java.util.HashMap;

public class H_Quack {
	
	private static final int MAX = 0xffff;
	
	static class QueueVector {
		private StackVector toPush;
		private StackVector toPop;
		
		QueueVector() {
			toPush = new StackVector();
			toPop = new StackVector();
		}
		
		void put(int i) {
			toPush.put(i);
		}
		
		
		int get() {
			if (toPop.size == 0) {
				int unIndexed = toPush.size;
				for (int i = 0; i < unIndexed; i++) {
					toPop.put(toPush.pop());
				}
			}
			return toPop.pop();
		}
		
		class StackVector {
			private int capacity;
			private int[] elements;
			private int size;
			
			StackVector() {
				capacity = 8;
				elements = new int[capacity];
				size = 0;
			}
			
			void put(int i) {
				if (size >= capacity) {
					capacity *= 2;
					int[] temp = new int[capacity];
					System.arraycopy(elements, 0, temp, 0, size);
					elements = temp;
				}
				
				elements[size++] = i;
			}
			
			int pop() {
				int last = elements[size - 1];
				elements[--size] = 0;
				if (size * 4 <= capacity && size > 8) {
					capacity /= 2;
					int[] temp = new int[capacity];
					System.arraycopy(elements, 0, temp, 0, size);
					elements = temp;
				}
				return last;
			}
			
		}
		
		@Override
		public String toString() {
			return "QueueMin{" +
					"toPush=" + toPush.toString() +
					", toPop=" + toPop.toString() +
					'}';
		}
		
		void add() {
			put((get() + get()) & MAX);
		}
		
		void subtract() {
			put((get() - get()) & MAX);
		}
		
		void multiply() {
			put((get() * get()) & MAX);
		}
		
		void divide() {
			int x = get();
			int y = get();
			if (x == 0 || y == 0) {
				put(0);
			} else {
				put(x / y);
			}
		}
		
		void mod() {
			int x = get();
			int y = get();
			if (x == 0 || y == 0) {
				put(0);
			} else {
				put(x % y);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		QueueVector a = new QueueVector();
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("quack.in")));
		PrintWriter out = new PrintWriter(new File("quack.out"));
		StringBuilder input = new StringBuilder();
		String readLine;
		while ((readLine = reader.readLine()) != null) {
			input.append(readLine).append(" ");
		}
		String[] cmds = input.toString().trim().split(" ");
		int[] registers = new int['z' - 'a' + 1];
		HashMap<String, Integer> labels = new HashMap<>();
		for (int i = 0; i < cmds.length; i++) {
			String cmd = cmds[i];
			if (cmd.charAt(0) == ':') {
				labels.put(cmd.substring(1), i);
			}
		}
		for (int i = 0; i < cmds.length; i++) {
			String cmd = cmds[i];
			try {
				int n = Integer.parseInt(cmd);
				a.put(n);
			} catch (NumberFormatException e) {
				char op = cmd.charAt(0);
				if (op == '+') {
					a.add();
				} else if (op == '-') {
					a.subtract();
				} else if (op == '*') {
					a.multiply();
				} else if (op == '/') {
					a.divide();
				} else if (op == '%') {
					a.mod();
				} else if (op == '>') { // Set register
					registers[cmd.charAt(1) - 'a'] = a.get();
				} else if (op == '<') { // Get register to queue
					a.put(registers[cmd.charAt(1) - 'a']);
				} else if (op == 'P') { // Print value
					if (cmd.length() > 1) {
						out.println(registers[cmd.charAt(1) - 'a']);
					} else {
						out.println(a.get());
					}
				} else if (op == 'C') { // Print char
					if (cmd.length() > 1) {
						out.print((char) (registers[cmd.charAt(1) - 'a'] & 0xff));
					} else {
						out.print((char) (a.get() & 0xff));
					}
				} else if (op == 'J') { // Go to label
					String label = cmd.substring(1);
					i = labels.get(label);
				} else if (op == 'Z') { // Go to if zero
					int reg = registers[cmd.charAt(1) - 'a'];
					if (reg == 0) {
						String label = cmd.substring(2);
						i = labels.get(label);
					}
				} else if (op == 'E') { // Go to if equal
					int reg1 = registers[cmd.charAt(1) - 'a'];
					int reg2 = registers[cmd.charAt(2) - 'a'];
					if (reg1 == reg2) {
						String label = cmd.substring(3);
						i = labels.get(label);
					}
				} else if (op == 'G') { // Go to if greater
					int reg1 = registers[cmd.charAt(1) - 'a'];
					int reg2 = registers[cmd.charAt(2) - 'a'];
					if (reg1 > reg2) {
						String label = cmd.substring(3);
						i = labels.get(label);
					}
				} else if (op == 'Q') { // End of program
					break;
				}
			}
		}
		out.close();
	}
}