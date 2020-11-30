import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class B_FMeasure {
	
	int k;
	int[][] confusion;
	
	public static void main(String[] args) throws IOException {
		new B_FMeasure().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		k = Integer.parseInt(in.readLine());
		confusion = new int[k][k];
		for (int i = 0; i < k; i++) {
			String[] line = in.readLine().trim().split(" +");
			for (int j = 0; j < k; j++) {
				confusion[i][j] = Integer.parseInt(line[j]);
			}
		}
		in.close();
		
		solve();
	}
	
	private void solve() {
		double[] precision = new double[k];
		double[] recall = new double[k];
		double[] f1 = new double[k];
		double f1weighted = 0;
		double pWeighted = 0;
		double rWeighted = 0;
		double samples = 0;
		for (int i = 0; i < k; i++) {
			int sumCol = 0;
			int sumRow = 0;
			for (int j = 0; j < k; j++) {
				sumCol += confusion[j][i];
				sumRow += confusion[i][j];
			}
			precision[i] = sumCol == 0 ? 0 : 1.0 * confusion[i][i] / sumCol;
			recall[i] = sumRow == 0 ? 0 : 1.0 * confusion[i][i] / sumRow;
			f1[i] = precision[i] + recall[i] == 0 ? 0 : 2 * precision[i] * recall[i] / (precision[i] + recall[i]);
			f1weighted += f1[i] * sumRow;
			pWeighted += precision[i] * sumRow;
			rWeighted += recall[i] * sumRow;
			samples += sumRow;
		}
		pWeighted /= samples;
		rWeighted /= samples;
		f1weighted /= samples;
		double macro = pWeighted + rWeighted < 1e-10 ? 0 : 2 * pWeighted * rWeighted / (pWeighted + rWeighted);
		double micro = f1weighted;
		System.out.println(macro);
		System.out.println(micro);
	}
	
}
