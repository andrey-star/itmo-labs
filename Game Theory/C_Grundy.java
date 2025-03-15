import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class C_Grundy {

    public static void dfs(int u, boolean[] used, List<Integer>[] g, List<Integer> t) {
        used[u] = true;
        for (int v : g[u]) {
            if (!used[v]) {
                dfs(v, used, g, t);
            }
        }
        t.add(u);
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int n = Integer.parseInt(line[0]);
        int m = Integer.parseInt(line[1]);
        List<Integer>[] g = new ArrayList[n];
        List<Integer>[] gRev = new ArrayList[n];
        for (int i = 0; i < n; i++) {
            g[i] = new ArrayList<>();
            gRev[i] = new ArrayList<>();
        }
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            int a = Integer.parseInt(line[0]) - 1;
            int b = Integer.parseInt(line[1]) - 1;
            g[a].add(b);
            gRev[b].add(a);
        }

        List<Integer> t = new ArrayList<>();
        boolean[] used = new boolean[n];
        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                dfs(i, used, g, t);
            }
        }
        int[] gr = new int[n];
        for (int u : t) {
            Set<Integer> grNext = new HashSet<>();
            for (int v : g[u]) {
                grNext.add(gr[v]);
            }
            for (int i = 0; i < n; i++) {
                if (!grNext.contains(i)) {
                    gr[u] = i;
                    break;
                }
            }
        }
        for (int i : gr) {
            System.out.println(i);
        }
    }

}
