import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class H_LogicalExpression {
	
	public static void main(String[] args) throws IOException {
		new H_LogicalExpression().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int m = Integer.parseInt(in.readLine());
		int[] f = new int[1 << m];
		int ones = 0;
		for (int i = 0; i < f.length; i++) {
			f[i] = Integer.parseInt(in.readLine());
			if (f[i] == 1) {
				ones++;
			}
		}
		if (ones > 512) {
			buildCnf(m, f);
		} else {
			buildDnf(m, f);
		}
	}
	
	private void buildDnf(int m, int[] f) {
		List<List<Double>> l1 = new ArrayList<>();
		for (int i = 0; i < f.length; i++) {
			if (f[i] == 1) {
				l1.add(andNeuron(m, i));
			}
		}
		if (l1.size() != 0) {
			System.out.println(2);
			System.out.println(l1.size() + " " + 1);
			for (List<Double> doubles : l1) {
				for (Double aDouble : doubles) {
					System.out.print(aDouble + " ");
				}
				System.out.println();
			}
			for (int i = 0; i < l1.size(); i++) {
				System.out.print(1.0 + " ");
			}
			System.out.println(-0.5);
		} else {
			System.out.println(1);
			System.out.println(1);
			for (int i = 0; i < m; i++) {
				System.out.print(1.0 + " ");
			}
			System.out.println(-0.5);
		}
	}
	
	private List<Double> identity(int m) {
		List<Double> res = new ArrayList<>();
		for (int i = 0; i < m; i++) {
			res.add(1.0);
		}
		res.add(0.0);
		return res;
	}
	
	private void buildCnf(int m, int[] f) {
		List<List<Double>> l1 = new ArrayList<>();
		for (int i = 0; i < f.length; i++) {
			if (f[i] == 0) {
				l1.add(orNeuron(m, i));
			}
		}
		System.out.println(2);
		System.out.println(l1.size() + " " + 1);
		for (List<Double> doubles : l1) {
			for (Double aDouble : doubles) {
				System.out.print(aDouble + " ");
			}
			System.out.println();
		}
		double sub = 0;
		for (int i = 0; i < l1.size(); i++) {
			System.out.print(1.0 + " ");
			sub += 1;
		}
		System.out.println(-sub + 0.5);
	}
	
	private List<Double> andNeuron(int m, int i) {
		List<Double> cons = new ArrayList<>();
		for (int j = 0; j < m; j++) {
			if (((i >> j) & 1) == 1) {
				cons.add(1.0);
			} else {
				cons.add(-1.0);
			}
		}
		cons.add(-Integer.bitCount(i) + 0.5);
		return cons;
	}
	
	private List<Double> orNeuron(int m, int i) {
		List<Double> cons = new ArrayList<>();
		for (int j = 0; j < m; j++) {
			if (((i >> j) & 1) == 1) {
				cons.add(-1.0);
			} else {
				cons.add(1.0);
			}
		}
		cons.add(Integer.bitCount(i) - 0.5);
		return cons;
	}
}
