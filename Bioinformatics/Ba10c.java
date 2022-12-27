import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Ba10c extends AbstractTask {
	
	private static final List<String> alphabet = new ArrayList<>();
	private static final List<String> states = new ArrayList<>();
	private static final Map<String, Map<String, Double>> transitions = new HashMap<>();
	private static final Map<String, Map<String, Double>> emissions = new HashMap<>();
	
	public Ba10c() {
		super("ba10c");
	}
	
	public static void main(String[] args) throws IOException {
		new Ba10c().test();
	}
	
	public void test(Path test) throws FileNotFoundException {
		alphabet.clear();
		states.clear();
		transitions.clear();
		emissions.clear();
		System.out.println(test.toFile());
		Scanner in = new Scanner(new InputStreamReader(new FileInputStream(test.toFile()))).useLocale(Locale.US);
		String x = in.next();
		in.nextLine();
		String[] line = in.nextLine().split(" ");
		int alphSize = line.length;
		alphabet.addAll(Arrays.asList(line));
		line = in.nextLine().split(" ");
		int sSize = line.length;
		states.addAll(Arrays.asList(line));
		for (int i = 0; i < sSize; i++) {
			Map<String, Double> a = new HashMap<>();
			for (int j = 0; j < sSize; j++) {
				a.put(states.get(j), in.nextDouble());
			}
			transitions.put(states.get(i), a);
		}
		for (int i = 0; i < sSize; i++) {
			Map<String, Double> a = new HashMap<>();
			for (int j = 0; j < alphSize; j++) {
				a.put(alphabet.get(j), in.nextDouble());
			}
			emissions.put(states.get(i), a);
		}
		System.out.println(solve(x));
	}
	
	private String solve(String x) {
		int n = x.length();
		int m = states.size();
		double[][] dp = new double[n][m];
		int[][] prev = new int[n][m];
		for (int i = 0; i < states.size(); i++) {
			dp[0][i] = emissions.get(states.get(i)).get("" + x.charAt(0));
		}
		for (int k = 1; k < n; k++) {
			for (int i = 0; i < m; i++) {
				double max = 0;
				int maxState = 0;
				for (int j = 0; j < m; j++) {
					double cur = dp[k - 1][j] * transitions.get(states.get(j)).get(states.get(i));
					if (cur > max) {
						max = cur;
						maxState = j;
					}
				}
				dp[k][i] = max * emissions.get(states.get(i)).get("" + x.charAt(k));
				prev[k][i] = maxState;
			}
		}
		int lastState = 0;
		for (int st = 0; st < states.size(); st++) {
			if (dp[n - 1][st] > dp[n - 1][lastState]) {
				lastState = st;
			}
		}
		
		List<Integer> res = new ArrayList<>();
		res.add(lastState);
		for (int t = n - 1; t > 0; t--) {
			int prevLs = prev[t][lastState];
			res.add(prevLs);
			lastState = prevLs;
		}
		Collections.reverse(res);
		return res.stream().map(states::get).collect(Collectors.joining());
	}
	
}
