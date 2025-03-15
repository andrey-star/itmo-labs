import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class D_Karlsson {

    private static Map<Integer, Integer> gs = new HashMap<>();

    private static int grundy(int n) {
        if (n == 1) {
            return 0;
        }
        if (!gs.containsKey(n)) {
            Set<Integer> sub = new HashSet<>();
            for (int i = 1; i <= n / 2; i++) {
                sub.add(grundy(n - i));
            }
            int i = 0;
            while (sub.contains(i)) {
                i++;
            }
            gs.put(n, i);
        }
        return gs.get(n);
    }

    private static int game(int a, int b, int c) {
        return grundy(a) ^ grundy(b) ^ grundy(c);
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int a = Integer.parseInt(line[0]);
        int b = Integer.parseInt(line[1]);
        int c = Integer.parseInt(line[2]);
        int res = game(a, b, c);
        if (res == 0) {
            System.out.println("NO");
            return;
        }
        System.out.println("YES");
        for (int i = 1; i <= a / 2; i++) {
            if (game(a - i, b, c) == 0) {
                System.out.println((a - i) + " " + b + " " + c);
                return;
            }
        }
        for (int i = 1; i <= b / 2; i++) {
            if (game(a, b - i, c) == 0) {
                System.out.println(a + " " + (b - i) + " " + c);
                return;
            }
        }
        for (int i = 1; i <= c / 2; i++) {
            if (game(a, b, c - i) == 0) {
                System.out.println(a + " " + b + " " + (c - i));
                return;
            }
        }
    }

}
