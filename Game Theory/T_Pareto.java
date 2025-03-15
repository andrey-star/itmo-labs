import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Random;

public class T_Pareto {

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

        boolean[][] notPareto = new boolean[m][n];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                outer:
                for (int i2 = 0; i2 < m; i2++) {
                    for (int j2 = 0; j2 < n; j2++) {
                        if ((a[i][j] <= a[i2][j2] && b[i][j] < b[i2][j2]) || (a[i][j] < a[i2][j2] && b[i][j] <= b[i2][j2])) {
                            notPareto[i][j] = true;
                            break outer;
                        }
                    }
                }
            }
        }

        StringBuilder sb = new StringBuilder();
        int res = 0;
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (!notPareto[i][j]) {
                    res++;
                    sb.append(i + 1).append(" ").append(j + 1).append("\n");
                }
            }
        }
        System.out.println(res);
        System.out.println(sb);
    }

}
