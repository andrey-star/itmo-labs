import java.io.IOException;
import java.util.ArrayList;

public class ReverseSum {
	
	private static void sum(ArrayList<ArrayList<Integer>> a) {
		int maxCols = 0;
		for (ArrayList<Integer> col : a) {
			if (maxCols < col.size()) {
				maxCols = col.size();
			}
		}
		
		int[] rowSum = new int[a.size()];
		int[] colSum = new int[maxCols];
		
		for (int i = 0; i < a.size(); i++) {
			for (int j = 0; j < a.get(i).size(); j++) {
				int el = a.get(i).get(j);
				rowSum[i] += el;
				colSum[j] += el;
			}
		}
		
		for (int i = 0; i < a.size(); i++) {
			for (int j = 0; j < a.get(i).size(); j++) {
				System.out.print(rowSum[i] + colSum[j] - a.get(i).get(j) + " ");
			}
			System.out.println();
		} 
	}
	
	public static void main(String[] args) throws IOException {
		ArrayList<ArrayList<Integer>> a = new ArrayList<>();
		try (FastScanner in = new FastScanner(System.in)) {
			while (in.hasNextLine()) {
				String line = in.readLine();
				ArrayList<Integer> num = new ArrayList<>();
				if (!line.equals("")) {
					String[] nextsInLine = in.split(line);
					for (String s : nextsInLine) {
						num.add(Integer.parseInt(s));
					}
				}
				a.add(num);
			}
			sum(a);
		} catch (NumberFormatException e) {
			System.out.println("Number format error");
		}

	}

}
