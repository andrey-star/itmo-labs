import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Collectors;

public class F_SnakesArrows {

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int n = Integer.parseInt(line[0]);
        int m = Integer.parseInt(line[1]);
        int k = Integer.parseInt(line[2]);
        int q = Integer.parseInt(line[3]);

        int[] chips = new int[k];
        line = in.readLine().trim().split(" +");
        for (int i = 0; i < k; i++) {
            chips[i] = Integer.parseInt(line[i]) - 1;
        }
        int[] port = new int[n];
        Arrays.fill(port, -1);
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            int start = Integer.parseInt(line[0]) - 1;
            int end = Integer.parseInt(line[1]) - 1;
            port[start] = end;
        }

        var g = buildGraph(n, q, port);
        int[] val = smith(g);
        String result = solve(val, chips, g);
        System.out.println(result);
        if (result.equals("Arkadii")) {
            for (int chipIndex = 0; chipIndex < chips.length; chipIndex++) {
                int chip = chips[chipIndex];
                for (int j = chip + 1; j <= Math.min(n - 1, chip + q); j++) {
                    int v;
                    if (port[j] != -1) {
                        v = port[j];
                    } else {
                        v = j;
                    }
                    chips[chipIndex] = v;
                    if (isNim0(chips, val)) {
                        System.out.println(chipIndex + 1 + " " + (j - chip));
                        return;
                    }
                    chips[chipIndex] = chip;
                }
            }
        }
    }

    private static boolean isNim0(int[] chips, int[] val) {
        int res = 0;
        for (int chip : chips) {
            if (val[chip] == -1) {
                return false;
            } else {
                res ^= val[chip];
            }
        }
        return res == 0;
    }

    private static String solve(int[] val, int[] chips, List<Integer>[] g) {
        int n = val.length;
        Set<Integer>[] val2 = new TreeSet[n];
        for (int i = 0; i < n; i++) {
            if (val[i] == -1) {
                Set<Integer> k = new TreeSet<>();
                for (int u : g[i]) {
                    if (val[u] != -1) {
                        k.add(val[u]);
                    }
                }
                val2[i] = k;
            }
        }
        boolean isNim = true;
        int nim = 0;
        Set<Integer> notNim = new HashSet<>();
        for (int chip : chips) {
            if (val[chip] != -1) {
                if (isNim) {
                    nim ^= val[chip];
                } else {
                    notNim = notNim.stream().map(i -> i ^ val[chip]).collect(Collectors.toSet());
                }
            } else {
                if (isNim) {
                    isNim = false;
                    int finalNim = nim;
                    notNim = val2[chip].stream().map(i -> i ^ finalNim).collect(Collectors.toSet());
                } else {
                    notNim = new HashSet<>();
                }
            }
        }
        if (isNim) {
            if (nim == 0) {
                return "Boris";
            } else {
                return "Arkadii";
            }
        } else {
            if (notNim.contains(0)) {
                return "Arkadii";
            } else {
                return "Draw";
            }
        }
    }

    private static List<Integer>[] buildGraph(int n, int q, int[] port) {
        List<Integer>[] g = new ArrayList[n];
        for (int i = 0; i < n; i++) {
            g[i] = new ArrayList<>();
        }
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j <= Math.min(n - 1, i + q); j++) {
                if (port[j] != -1) {
                    g[i].add(port[j]);
                } else {
                    g[i].add(j);
                }
            }
        }
        return g;
    }

    private static int[] smith(List<Integer>[] g) {
        int n = g.length;
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
        return val;

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
