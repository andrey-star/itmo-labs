import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Set;

public class N_Saddle {

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int m = Integer.parseInt(line[0]);
        int n = Integer.parseInt(line[1]);
        int[][] a = new int[m][n];
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            for (int j = 0; j < n; j++) {
                a[i][j] = Integer.parseInt(line[j]);
            }
        }


        boolean[][] notNash = new boolean[m][n];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                for (int i2 = 0; i2 < m; i2++) {
                    if (a[i][j] < a[i2][j]) {
                        notNash[i][j] = true;
                        break;
                    }
                }
                for (int j2 = 0; j2 < n; j2++) {
                    if (a[i][j] > a[i][j2]) {
                        notNash[i][j] = true;
                        break;
                    }
                }
            }
        }
        Set<Integer> first = new HashSet<>();
        Set<Integer> second = new HashSet<>();
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (!notNash[i][j]) {
                    first.add(i + 1);
                    second.add(j + 1);
                }
            }
        }
        System.out.println(first.size() + " " + second.size());
        for (int i : first) {
            System.out.print(i + " ");
        }
        System.out.println();
        for (int i : second) {
            System.out.print(i + " ");
        }
    }

}
