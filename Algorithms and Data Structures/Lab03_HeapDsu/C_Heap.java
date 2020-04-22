import java.io.*;
import java.util.ArrayList;

public class C_Heap {
	
	static class Pair {
		int value;
		int index;
		
		Pair(int value, int index) {
			this.value = value;
			this.index = index;
		}
		
		@Override
		public String toString() {
			return value + " " + (index + 1);
		}
	}
	
	private int size;
	private final ArrayList<Pair> heap;
	private final ArrayList<Integer> opNumToPosition;
	
	private C_Heap() {
		size = 0;
		heap = new ArrayList<>();
		opNumToPosition = new ArrayList<>();
	}
	
	private void push(int val, int index) {
		heap.add(new Pair(val, index));
		while (opNumToPosition.size() <= index) {
			opNumToPosition.add(-1);
		}
		opNumToPosition.set(index, heap.size() - 1);
		siftUp(size++);
	}
	
	private void swapPair(int i, int j) {
		Pair temp = heap.get(i);
		heap.set(i, heap.get(j));
		heap.set(j, temp);
	}
	
	private void swapInt(int i, int j) {
		int dummy = opNumToPosition.get(i);
		opNumToPosition.set(i, opNumToPosition.get(j));
		opNumToPosition.set(j, dummy);
	}
	
	private void siftUp(int i) {
		if (heap.get((i - 1) / 2).value > heap.get(i).value) {
			swapPair((i - 1) / 2, i);
			swapInt(heap.get((i - 1) / 2).index, heap.get(i).index);
			siftUp((i - 1) / 2);
		}
	}
	
	private Pair extractMin() {
		Pair result = heap.get(0);
		heap.set(0, heap.get(size - 1));
		opNumToPosition.set(heap.get(size - 1).index, 0);
		opNumToPosition.set(result.index, -1);
		heap.remove(size - 1);
		size--;
		siftDown(0);
		return result;
	}
	
	private void decreaseKey(int x, int v) {
		if (opNumToPosition.get(x) != -1) {
			int indexInHeap = opNumToPosition.get(x);
			Pair p = heap.get(indexInHeap);
			heap.set(indexInHeap, new Pair(v, p.index));
			while (opNumToPosition.size() <= x) {
				opNumToPosition.add(-1);
			}
			opNumToPosition.set(x, indexInHeap);
			siftUp(indexInHeap);
		}
	}
	
	private void siftDown(int i) {
		if (2 * i + 1 < size) {
			int left = 2 * i + 1;
			int right = 2 * i + 2;
			int j = left;
			if (right < size && heap.get(right).value <= heap.get(left).value) {
				j = right;
			}
			if (heap.get(i).value > heap.get(j).value) {
				swapPair(i, j);
				swapInt(heap.get(i).index, heap.get(j).index);
				siftDown(j);
			}
		}
	}
	
	private boolean isEmpty() {
		return size == 0;
	}
	
	@Override
	public String toString() {
		return "Heap{" +
				"size=" + size +
				", heap=" + heap +
				'}';
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("priorityqueue2.in")));
		C_Heap heap = new C_Heap();
		String line;
		int lineIndex = 0;
		PrintWriter out = new PrintWriter(new File("priorityqueue2.out"));
		while ((line = in.readLine()) != null) {
			String[] cmd = line.trim().split(" +");
			if (cmd[0].equals("push")) {
				heap.push(Integer.parseInt(cmd[1]), lineIndex);
			} else if (cmd[0].equals("decrease-key")) {
				heap.decreaseKey(Integer.parseInt(cmd[1]) - 1, Integer.parseInt(cmd[2]));
			} else if (cmd[0].equals("extract-min")) {
				if (!heap.isEmpty()) {
					out.println(heap.extractMin());
				} else {
					out.println("*");
				}
			}
			lineIndex++;
		}
		out.close();
	}
	
}
