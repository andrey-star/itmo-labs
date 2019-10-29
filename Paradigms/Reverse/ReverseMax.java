import java.util.*;

public class ReverseMax {

	public static void max(ArrayList<ArrayList<Integer>> a) {
		
		int maxCols = 0;
		for (int i = 0; i < a.size(); i++) {
			if (maxCols < a.get(i).size()) {
				maxCols = a.get(i).size();
			}
		}
		
		int[] rowMax = new int[a.size()];
		int[] colMax = new int[maxCols];
		Arrays.fill(rowMax, Integer.MIN_VALUE);
		Arrays.fill(colMax, Integer.MIN_VALUE);
		
		for (int i = 0; i < a.size(); i++) {
			for (int j = 0; j < a.get(i).size(); j++) {
				int el = a.get(i).get(j);
				rowMax[i] = Math.max(rowMax[i], el);
				colMax[j] = Math.max(colMax[j], el);
			}
		}
		
		for (int i = 0; i < a.size(); i++) {
			for (int j = 0; j < a.get(i).size(); j++) {
				System.out.print(Math.max(rowMax[i], colMax[j]) + " ");
			}
			System.out.println();
		} 
	}
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		ArrayList<ArrayList<Integer>> a = new ArrayList<>();
		while (in.hasNextLine()) {
			String line = in.nextLine();
			ArrayList<Integer> num = new ArrayList<>();
			if (!line.equals("")) {
				for (String s : line.split(" ")) {
					num.add(Integer.parseInt(s));
				}
			}
			a.add(num);
		}
		in.close();
		max(a);

	}

}
