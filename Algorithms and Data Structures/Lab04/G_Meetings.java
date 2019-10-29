import java.io.*;
import java.util.Arrays;

public class G_Meetings {
	
	private static class Meeting {
		int min;
		int max;
		int change;
		
		private Meeting(int min, int max, int change) {
			this.min = min;
			this.max = max;
			this.change = change;
		}
	}
	
	private static void dp(int[] dp, int[] mood, int[] take, Meeting[] meetings, int start, int n) {
		dp[0] = 0;
		take[0] = -1;
		mood[0] = start;
		for (int i = 1; i < 1 << n; i++) {
			int maxIndex = -1;
			for (int j = 0; j < n; j++) {
				if (((i >> j) & 1) == 1) {
					if (meetings[j].min <= mood[i - (1 << j)] && meetings[j].max >= mood[i - (1 << j)]) {
						if (dp[i] < dp[i - (1 << j)]) {
							maxIndex = j;
							mood[i] = mood[i - (1 << j)] + meetings[j].change;
						}
					}
				}
			}
			if (maxIndex != -1) {
				take[i] = maxIndex;
				dp[i] = dp[i - (1 << maxIndex)] + 1;
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("meetings.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		Meeting[] meetings = new Meeting[n];
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			int c = Integer.parseInt(line[2]);
			meetings[i] = new Meeting(a, b, c);
		}
		in.close();
		
		int[] dp = new int[1 << n];
		int[] mood = new int[1 << n];
		int[] take = new int[1 << n];
		Arrays.fill(dp, -1);
		dp(dp, mood, take, meetings, k, n);
		int maxIndex = 0;
		for (int i = 0; i < dp.length; i++) {
			maxIndex = dp[i] > dp[maxIndex] ? i : maxIndex;
		}
		int[] res = new int[dp[maxIndex]];
		int mask = maxIndex;
		int cur = dp[maxIndex];
		while (mask != 0) {
			res[--cur] = take[mask] + 1;
			mask -= (1 << take[mask]);
		}
		PrintWriter out = new PrintWriter(new File("meetings.out"));
		out.println(dp[maxIndex]);
		for (int i : res){
			out.print(i + " ");
		}
		out.close();
	}
}