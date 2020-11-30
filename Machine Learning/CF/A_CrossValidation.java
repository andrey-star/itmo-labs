import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class A_CrossValidation {
	
	public static void main(String[] args) throws IOException {
		new A_CrossValidation().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		List<Integer>[] index = new List[m];
		for (int i = 0; i < m; i++) {
			index[i] = new ArrayList<>();
		}
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			int c = Integer.parseInt(line[i]) - 1;
			index[c].add(i);
		}
		List<Integer>[] res = new List[k];
		for (int i = 0; i < k; i++) {
			res[i] = new ArrayList<>();
		}
		int cur = 0;
		for (int i = 0; i < m; i++) {
			for (int ind : index[i]) {
				res[cur % k].add(ind + 1);
				cur++;
			}
		}
		
		for (List<Integer> re : res) {
			System.out.println(re.size() + " " + re.stream().map(i -> i + "").collect(Collectors.joining(" ")));
		}
	}
}
