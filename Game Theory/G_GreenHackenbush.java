import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

@SuppressWarnings("unchecked")
public class G_GreenHackenbush {

    private static int colors = 0;
    private static boolean[] bridge;

    private static void dfs(List<Edge>[] g, int u, boolean[] used, int timer, int[] tin, int[] up, int prevEdgeNum) {
        timer++;
        tin[u] = timer;
        up[u] = timer;
        used[u] = true;
        for (Edge edge : g[u]) {
            int v = edge.to;
            int curEdgeNum = edge.edgeNum;
            if (!used[v]) {
                dfs(g, v, used, timer, tin, up, curEdgeNum);
                bridge[curEdgeNum] = up[v] > tin[u];
                up[u] = Math.min(up[u], up[v]);
            } else if (curEdgeNum != prevEdgeNum) {
                up[u] = Math.min(up[u], tin[v]);
            }
        }
    }

    private static class Edge {
        int to;
        int edgeNum;

        public Edge(int to, int edgeNum) {
            this.to = to;
            this.edgeNum = edgeNum;
        }
    }

    private static void paint(List<Edge>[] g, int u, int curColor, int[] color) {
        color[u] = curColor;
        for (Edge e : g[u]) {
            int v = e.to;
            if (color[v] == 0 && !bridge[e.edgeNum]) {
                paint(g, v, curColor, color);
            }
        }
    }

    public static void main(String[] args) throws IOException {
        solve();
    }


    private static class Pair {
        int a;
        int b;

        public Pair(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }

    private static void solve() throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        int n = Integer.parseInt(line[0]);
        int m = Integer.parseInt(line[1]);

        List<Edge>[] g = new ArrayList[n];
        List<Pair> edges = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            g[i] = new ArrayList<>();
        }
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            int a = Integer.parseInt(line[0]) - 1;
            int b = Integer.parseInt(line[1]) - 1;

            g[a].add(new Edge(b, i));
            g[b].add(new Edge(a, i));

            edges.add(new Pair(a, b));
        }
        in.close();

        bridge = new boolean[m];

        boolean[] used = new boolean[n];
        int[] tin = new int[n];
        int[] up = new int[n];

        int timer = 0;
        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                dfs(g, i, used, timer, tin, up, -1);
            }
        }
        int[] color = new int[n];
        for (int i = 0; i < n; i++) {
            if (color[i] == 0) {
                paint(g, i, ++colors, color);
            }
        }

        for (int i = 0; i < color.length; i++) {
            color[i]--;
        }

        int[] edgesInComp = new int[colors];
        for (Pair edge : edges) {
            if (color[edge.a] == color[edge.b]) {
                edgesInComp[color[edge.a]]++;
            }
        }

        List<Integer>[] tree = new ArrayList[colors];
        for (int i = 0; i < colors; i++) {
            tree[i] = new ArrayList<>();
        }
        for (int u = 0; u < n; u++) {
            for (Edge e : g[u]) {
                int v = e.to;
                if (color[u] != color[v]) {
                    tree[color[u]].add(color[v]);
                }
            }
        }
        int res = tree(color[0], tree, new boolean[n], edgesInComp);
        System.out.println(res == 0 ? "Second" : "First");
    }

    private static int tree(int u, List<Integer>[] g, boolean[] used, int[] edgesInComp) {
        used[u] = true;
        int res = 0;
        for (int v : g[u]) {
            if (!used[v]) {
                res ^= tree(v, g, used, edgesInComp) + 1;
            }
        }
        if (edgesInComp[u] % 2 == 1) {
            res ^= 1;
        }
        return res;
    }
}
