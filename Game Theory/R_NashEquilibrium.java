import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class R_NashEquilibrium {

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int m = Integer.parseInt(line[0]);
        int n = Integer.parseInt(line[1]);
        int[][] a = new int[m][n];
        int[][] b = new int[m][n];
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            for (int j = 0; j < n; j++) {
                a[i][j] = Integer.parseInt(line[j]);
            }
        }
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            for (int j = 0; j < n; j++) {
                b[i][j] = Integer.parseInt(line[j]);
            }
        }

        boolean[][] notNash = new boolean[m][n];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                // i, j - nash => i, j - max i', j
                for (int i2 = 0; i2 < m; i2++) {
                    if (a[i][j] < a[i2][j]) {
                        notNash[i][j] = true;
                        break;
                    }
                }
                for (int j2 = 0; j2 < n; j2++) {
                    if (b[i][j] < b[i][j2]) {
                        notNash[i][j] = true;
                        break;
                    }
                }
            }
        }
        StringBuilder sb = new StringBuilder();
        int res = 0;
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (!notNash[i][j]) {
                    res++;
                    sb.append(i + 1).append(" ").append(j + 1).append("\n");
                }
            }
        }
        System.out.println(res);
        System.out.println(sb);
    }

}
