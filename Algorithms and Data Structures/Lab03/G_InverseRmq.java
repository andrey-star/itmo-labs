import java.io.*;
import java.util.*;

public class G_InverseRmq {
	
	static class Heap {
		
		static class Pair {
			int value;
			int index;
			
			Pair(int value, int index) {
				this.value = value;
				this.index = index;
			}
			
		}
		
		private int size;
		private Pair[] heap;
		private int[] opNumToPosition;
		
		private Heap() {
			size = 0;
			heap = new Pair[(int) 3e5];
			opNumToPosition = new int[(int) 3e5];
		}
		
		private void push(int val, int index) {
			heap[size] = new Pair(val, index);
			opNumToPosition[index] = size;
			siftUp(size++);
		}
		
		private void swapPair(int i, int j) {
			Pair temp = heap[i];
			heap[i] = heap[j];
			heap[j] = temp;
		}
		
		private void swapInt(int i, int j) {
			int dummy = opNumToPosition[i];
			opNumToPosition[i] = opNumToPosition[j];
			opNumToPosition[j] = dummy;
		}
		
		private void remove() {
			Pair result = heap[0];
			heap[0] = heap[size - 1];
			opNumToPosition[heap[size - 1].index] = 0;
			opNumToPosition[result.index] = -1;
			size--;
			siftDown(0);
		}
		
		private void remove(int x) {
			decreaseKey(x, Integer.MAX_VALUE);
			remove();
		}
		
		private void decreaseKey(int x, int v) {
			if (opNumToPosition[x] != -1) {
				int indexInHeap = opNumToPosition[x];
				Pair p = heap[indexInHeap];
				heap[indexInHeap] = new Pair(v, p.index);
				opNumToPosition[x] = indexInHeap;
				siftUp(indexInHeap);
			}
		}
		
		private void siftUp(int i) {
			while (heap[(i - 1) / 2].value < heap[i].value) {
				swapPair((i - 1) / 2, i);
				swapInt(heap[(i - 1) / 2].index, heap[i].index);
				i = (i - 1) / 2;
			}
		}
		
		private void siftDown(int i) {
			while (2 * i + 1 < size) {
				int left = 2 * i + 1;
				int right = 2 * i + 2;
				int j = left;
				if (right < size && heap[right].value >= heap[left].value) {
					j = right;
				}
				if (heap[i].value < heap[j].value) {
					swapPair(i, j);
					swapInt(heap[i].index, heap[j].index);
					i = j;
				} else {
					break;
				}
			}
		}
		
		private int peek() {
			return heap[0].value;
		}
		
		private boolean isEmpty() {
			return size == 0;
		}
		
	}
	
	static class Operation {
		int val;
		String type;
		int opIndex;
		
		Operation(int val, int opIndex, String type) {
			this.val = val;
			this.type = type;
			this.opIndex = opIndex;
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("rmq.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int[] a = new int[n];
		//noinspection unchecked
		ArrayList<Operation>[] operations = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			operations[i] = new ArrayList<>();
		}
		Heap heap = new Heap();
		for (int k = 0; k < m; k++) {
			line = in.readLine().trim().split(" +");
			int left = Integer.parseInt(line[0]) - 1;
			int right = Integer.parseInt(line[1]) - 1;
			int value = Integer.parseInt(line[2]);
			operations[left].add(new Operation(value, k, "add"));
			if (right + 1 < n) {
				operations[right + 1].add(new Operation(value, k, "rem"));
			}
		}
		
		for (int i = 0; i < n; i++) {
			for (Operation op : operations[i]) {
				if (op.type.equals("add")) {
					heap.push(op.val, op.opIndex);
				} else if (op.type.equals("rem")) {
					heap.remove(op.opIndex);
				}
			}
			if (!heap.isEmpty()) {
				a[i] = heap.peek();
			}
		}
		PrintWriter out = new PrintWriter(new File("rmq.out"));
		for (int i : a) {
			out.print(i + " ");
		}
		out.close();
	}
	
}
