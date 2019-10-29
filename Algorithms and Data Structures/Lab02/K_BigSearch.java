import java.util.Scanner;

public class K_BigSearch {
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		long x = in.nextLong();
		int m = in.nextInt();
		System.out.println("? 1");
		long first = in.nextLong();
		x = x - (first - 1);
		if (x < 1) {
			x += (long) 1e18;
		}
		long l = Math.max(x - m, 1);
		long r = Math.min(x, (long) 1e18 - m);
		long ans = -1;
		while (l <= r) {
			long mid = l + (r - l) / 2;
			System.out.println("? " + mid);
			long res = in.nextLong();
			res = res - (first - 1);
			if (res < 1) {
				res += (long) 1e18;
			}
			if (res == x) {
				ans = mid;
				break;
			} else {
				if (res > x) {
					r = mid - 1;
				} else {
					l = mid + 1;
				}
			}
		}
		System.out.println("! " + ans);
		in.close();
	}
	
}
