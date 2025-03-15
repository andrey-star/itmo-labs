import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class A_GraphGame {

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

        int[] z = new int[n];
        Set<Integer> ps = new HashSet<>();
        Set<Integer> ns = new HashSet<>();
        for (int i = 0; i < n; i++) {
            z[i] = g[i].size();
            if (z[i] == 0) {
                ps.add(i);
            }
        }
        Queue<Integer> q = new ArrayDeque<>(ps);
        while (!q.isEmpty()) {
            int u = q.poll();
            if (ps.contains(u)) {
                for (int v : gRev[u]) {
                    if (!ns.contains(v)) {
                        ns.add(v);
                        q.add(v);
                    }
                }
            } else if (ns.contains(u)) {
                for (int v : gRev[u]) {
                    z[v]--;
                    if (z[v] == 0) {
                        ps.add(v);
                        q.add(v);
                    }
                }
            }
        }
        for (int i = 0; i < n; i++) {
            if (ps.contains(i)) {
                System.out.println("Loss");
            } else if (ns.contains(i)) {
                System.out.println("Win");
            } else {
                System.out.println("Draw");
            }
        }
    }

}
