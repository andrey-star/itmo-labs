import java.io.*;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class A_Planning {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("schedule.in")));
		int n = Integer.parseInt(in.readLine());
		Task[] tasks = new Task[n];
		String[] line;
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int d = Integer.parseInt(line[0]);
			int w = Integer.parseInt(line[1]);
			tasks[i] = new Task(d, w);
		}
		Arrays.sort(tasks, Comparator.comparingInt((Task t) -> t.w).reversed());
		
		NavigableSet<Integer> time = Stream.iterate(1, a -> a + 1)
				.limit(n)
				.collect(Collectors.toCollection(TreeSet::new));
		
		long res = 0;
		for (int i = 0; i < n; i++) {
			int d = tasks[i].d;
			if (time.first() > d) {
				time.remove(time.last());
				res += tasks[i].w;
			} else {
				time.remove(time.floor(d));
			}
		}
		
		PrintWriter out = new PrintWriter(new File("schedule.out"));
		out.println(res);
		out.close();
	}
	
	private static class Task {
		final int d;
		final int w;
		
		Task(int d, int w) {
			this.d = d;
			this.w = w;
		}
	}
	
}
