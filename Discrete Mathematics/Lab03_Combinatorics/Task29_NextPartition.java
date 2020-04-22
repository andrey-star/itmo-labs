import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

public class Task29_NextPartition {
	
	private static ArrayList<Integer> nextPartition(int[] arr) {
		ArrayList<Integer> a = new ArrayList<>();
		for (int i : arr) {
			a.add(i);
		}
		int last = a.size() - 1;
		a.set(last, a.get(last) - 1);
		a.set(last - 1, a.get(last - 1) + 1);
		if (a.get(last) >= a.get(last - 1)) {
			while (a.get(last) >= a.get(last - 1) * 2) {
				int diff = a.get(last) - a.get(last - 1);
				a.set(last, a.get(last - 1));
				a.add(diff);
				last++;
			}
		} else {
			a.set(last - 1, a.get(last - 1) + a.get(last));
			a.remove(last);
		}
		return a;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("nextpartition.in"));
		String s = in.next();
		String[] spl = s.trim().split("=");
		int n = Integer.parseInt(spl[0]);
		s = s.substring(s.indexOf('=') + 1);
		spl = s.split("\\+");
		int[] a = new int[spl.length];
		for (int i = 0; i < spl.length; i++) {
			a[i] = Integer.parseInt(spl[i]);
		}
		PrintWriter out = new PrintWriter(new File("nextpartition.out"));
		if (a.length == 1) {
			out.println("No solution");
		} else {
			out.print(n + "=");
			ArrayList<Integer> list = nextPartition(a);
			for (int i = 0; i < list.size(); i++) {
				out.print(list.get(i));
				if (i != list.size() - 1) {
					out.print("+");
				}
			}
		}
		out.close();
	}
	
}