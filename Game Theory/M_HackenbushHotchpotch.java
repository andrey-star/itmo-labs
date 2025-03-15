import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class M_HackenbushHotchpotch {
    static List<Edge>[] g;
    static List<Edge>[] gRev;
    static int[] edgeColors;
    static int[] reducedMasks;
    static int n;
    static int m;

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String[] line = in.readLine().trim().split(" +");
        n = Integer.parseInt(line[0]);
        m = Integer.parseInt(line[1]);
        g = new ArrayList[n];
        gRev = new ArrayList[n];
        edgeColors = new int[m];
        for (int i = 0; i < n; i++) {
            g[i] = new ArrayList<>();
            gRev[i] = new ArrayList<>();
        }
        for (int i = 0; i < m; i++) {
            line = in.readLine().trim().split(" +");
            int a = Integer.parseInt(line[0]) - 1;
            int b = Integer.parseInt(line[1]) - 1;
            int c = Integer.parseInt(line[2]);
            g[a].add(new Edge(b, c, i));
            g[b].add(new Edge(a, c, i));
            edgeColors[i] = c;
        }
        int masks = 1 << m;
        reducedMasks = new int[masks];
        Arrays.fill(reducedMasks, -1);
        boolean[][] dp = new boolean[masks][2]; // dp[mask][0] - blue wins in subgraph with blue (left) going first.

        for (int mask = 0; mask < masks; mask++) {
            boolean blueHasWinningMove = false;
            boolean redHasWinningMove = false;
            for (int deleteEdge = 0; deleteEdge < m; deleteEdge++) {
                if (getMask(deleteEdge, mask)) {
                    int deletedMask = reduceMask(dropMask(deleteEdge, mask));
                    if (edgeColors[deleteEdge] != 1 && dp[deletedMask][1]) {
                        blueHasWinningMove = true;
                    }
                    if (edgeColors[deleteEdge] != 0 && !dp[deletedMask][0]) {
                        redHasWinningMove = true;
                    }
                }
            }
            dp[mask][0] = blueHasWinningMove;
            dp[mask][1] = !redHasWinningMove;
        }
        System.out.print(dp[masks - 1][0] ? "Left" : "Right");
        System.out.print(" ");
        System.out.println(dp[masks - 1][1] ? "Left" : "Right");
    }

    private static int reduceMask(int mask1) {
        if (reducedMasks[mask1] == -1) {
            int newMask = 0;
            boolean[] used = new boolean[n];
            Queue<Integer> q = new ArrayDeque<>();
            q.add(0);
            while (!q.isEmpty()) {
                int u = q.poll();
                used[u] = true;
                for (Edge edge : g[u]) {
                    int v = edge.to;
                    if (getMask(edge.index, mask1)) {
                        newMask = setMask(edge.index, newMask);
                        if (!used[v]) {
                            q.add(v);
                        }
                    }
                }
            }
            reducedMasks[mask1] = newMask;
        }
        return reducedMasks[mask1];
    }

    private static boolean getMask(int i, int mask) {
        return ((mask >> i) & 1) == 1;
    }

    private static int setMask(int i, int mask) {
        return mask | (1 << i);
    }

    private static int dropMask(int i, int mask) {
        return mask & ~(1 << i);
    }

    private static class Edge {
        int to;
        int color;
        int index;

        public Edge(int to, int color, int index) {
            this.to = to;
            this.color = color;
            this.index = index;
        }
    }
}
