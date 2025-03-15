import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class E_Smith {

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

        int[] val = new int[n];
        Arrays.fill(val, -1);
        boolean changed = true;
        while (changed) {
            changed = false;
            for (int u = 0; u < n; u++) {
                if (val[u] != -1) {
                    continue;
                }
                if (g[u].isEmpty()) {
                    val[u] = 0;
                    changed = true;
                    continue;
                }
                Set<Integer> notDone = new HashSet<>();
                Set<Integer> done = new HashSet<>();
                for (int v : g[u]) {
                    if (val[v] == -1) {
                        notDone.add(v);
                    } else {
                        done.add(val[v]);
                    }
                }
                int mex = mex(done, n);
                if (notDone.isEmpty()) {
                    val[u] = mex;
                    changed = true;
                } else {
                    boolean allHaveMexChild = true;
                    for (int v : notDone) {
                        boolean hasMexChild = false;
                        for (int f : g[v]) {
                            if (val[f] == mex) {
                                hasMexChild = true;
                                break;
                            }
                        }
                        if (!hasMexChild) {
                            allHaveMexChild = false;
                            break;
                        }
                    }
                    if (allHaveMexChild) {
                        val[u] = mex;
                        changed = true;
                    }
                }
            }
        }
        for (int i = 0; i < n; i++) {
            if (val[i] != -1) {
                System.out.println(val[i]);
            } else {
                Set<Integer> k = new TreeSet<>();
                for (int u : g[i]) {
                    if (val[u] != -1) {
                        k.add(val[u]);
                    }
                }
                System.out.print(-1 + " " + k.size() + " ");
                for (int a : k) {
                    System.out.print(a + " ");
                }
                System.out.println();
            }
        }
    }

    private static int mex(Set<Integer> vals, int max) {
        for (int i = 0; i <= max; i++) {
            if (!vals.contains(i)) {
                return i;
            }
        }
        throw new AssertionError("mex???");
    }

}
