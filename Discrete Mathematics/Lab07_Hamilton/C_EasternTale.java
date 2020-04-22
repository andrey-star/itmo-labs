import java.util.*;

public class C_EasternTale {
	private static Scanner in;
	
	public static void main(String[] args) {
		in = new Scanner(System.in);
		int n = in.nextInt();
		List<Integer> res = new ArrayList<>();
		res.add(1);
		for (int i = 2; i <= n; i++) {
			int index = binarySearch(res, i);
			res.add(index, i);
		}
		System.out.print(0 + " ");
		for (int i = 0; i < n; i++) {
			System.out.print(res.get(i) + " ");
		}
	}
	
	private static int binarySearch(List<Integer> res, int val) {
		int l = -1;
		int r = res.size();
		while (l + 1 < r) {
			int mid = (l + r) / 2;
			System.out.println(1 + " " + res.get(mid) + " " + val);
			System.out.flush();
			
			String resp = in.next();
			if (resp.equals("YES")) {
				l = mid;
			} else {
				r = mid;
			}
		}
		return r;
	}
	
}