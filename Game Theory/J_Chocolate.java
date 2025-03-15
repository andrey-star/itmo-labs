import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class J_Chocolate {

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int n = Integer.parseInt(line[0]);
        int[] a = new int[n];
        int[] b = new int[n];
        for (int i = 0; i < n; i++) {
            line = in.readLine().trim().split(" +");
            a[i] = Integer.parseInt(line[0]);
            b[i] = Integer.parseInt(line[1]);
        }
        int res = 0;
        for (int i = 0; i < n; i++) {
            res += v(a[i], b[i]);
        }
        System.out.println(res > 0 ? "Vova" : "Gena");
    }

    private static int v(int a, int b) {
        if (a > b) {
            return -v(b, a);
        }
        return (int) Math.floor(b / Math.pow(2, (pow2(a) - 1))) - 1;
    }

    private static int pow2(int a) {
        int res = 0;
        while ((1 << res) <= a) {
            res++;
        }
        return res;
    }

}
