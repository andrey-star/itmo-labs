#include <bits/stdc++.h>

using namespace std;

const int nn = (int) 1e5 + 10;
const int log_nn = 20;
int timer = 0;

int tin[nn], tout[nn], d[nn];

vector<int> g[nn];

int st[nn][log_nn];

void reset() {
    timer = 0;
    for (auto &i : g) {
        i.clear();
    }
}

bool is_ancestor(int a, int b) {
    return tin[a] <= tin[b] && tout[a] >= tout[b];
}

int lca(int a, int b, int st_size) {
    if (is_ancestor(a, b)) {
        return a;
    }
    if (is_ancestor(b, a)) {
        return b;
    }
    for (int i = st_size - 1; i >= 0; i--) {
        if (!is_ancestor(st[a][i], b)) {
            a = st[a][i];
        }
    }
    return st[a][0];
}

int lca_root(int a, int b, int root, int st_size) {
    int a_root = lca(a, root, st_size);
    int b_root = lca(b, root, st_size);

    int a_root_path = d[a_root] + d[root] - 2 * d[lca(a_root, root, st_size)];
    int b_root_path = d[b_root] + d[root] - 2 * d[lca(b_root, root, st_size)];

    if (a_root_path < b_root_path) {
        return a_root;
    }
    if (a_root_path > b_root_path) {
        return b_root;
    }
    return lca(a, b, st_size);
}


void dfs(int u, int depth, int parent, int st_size) {
    tin[u] = timer;
    timer++;
    st[u][0] = parent;
    d[u] = depth;
    for (int i = 1; i < st_size; i++) {
        st[u][i] = st[st[u][i - 1]][i - 1];
    }
    for (int v : g[u]) {
        if (v != parent) {
            dfs(v, depth + 1, u, st_size);
        }
    }
    tout[u] = timer - 1;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    while (true) {
        int n, m;
        cin >> n;
        if (n == 0) {
            break;
        }
        reset();
        int root = 0;
        for (int i = 0; i < n - 1; ++i) {
            int u, v;
            cin >> u >> v;
            g[u - 1].push_back(v - 1);
            g[v - 1].push_back(u - 1);
        }
        int log_n = 1;
        while ((1 << log_n) <= n) {
            log_n++;
        }
        dfs(0, 0, 0, log_n + 1);

        cin >> m;
        string cmd;
        for (int i = 0; i < m; i++) {
            cin >> cmd;
            if (cmd == "?") {
                int u, v;
                cin >> u >> v;
                cout << lca_root(u - 1, v - 1, root, log_n + 1) + 1 << "\n";
            } else {
                cin >> root;
                root--;
            }
        }
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
