import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class H_RootNim {

    private static long grundy(long n) {
        long l = (long) Math.floor(Math.sqrt(n));
        long r = l + 1;
        long diff = n - l * l;
        if (diff >= r) {
            diff -= r;
        }
        if (diff % 2 == 0) {
            return l - (diff / 2);
        } else {
            return bitShit((r - diff) / 2 + l / 2 - ((l % 2 == 0) ? 1 : 0));
        }
    }

    private static long bitShit(long a) {
        long ones = Long.numberOfTrailingZeros(~a);
        return a >> (ones + 1);
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int n = Integer.parseInt(line[0]);
        long[] s = new long[n];
        if (n > 0) {
            line = in.readLine().trim().split(" +");
            for (int i = 0; i < n; i++) {
                s[i] = Long.parseLong(line[i]);
            }
        }
        int res = 0;
        for (int i = 0; i < n; i++) {
            res ^= grundy(s[i]);
        }
        if (res == 0) {
            System.out.println("Second");
        } else {
            System.out.println("First");
        }
    }

}
