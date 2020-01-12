#include <bits/stdc++.h>

using namespace std;

int left(int i) {
    return 2 * i + 1;
}

int right(int i) {
    return left(i) + 1;
}

int parent(int i) {
    return (i - 1) / 2;
}

long long query(int node, int a, int b, int l, int r, vector<long long> const &t) {
    if (l >= b || r <= a) {
        return 0;
    }
    if (l >= a && r <= b) {
        return t[node];
    }
    int m = (l + r) / 2;
    return query(left(node), a, b, l, m, t) + query(right(node), a, b, m, r, t);
}

long long query(int a, int b, vector<long long> const &t) {
    return query(0, a, b, 0, static_cast<int>((t.size() + 1) / 2), t);
}

void update(int i, vector<long long> &t) {
    t[i] = t[left(i)] + t[right(i)];
    if (i == 0) {
        return;
    }
    update(parent(i), t);
}

void add_v(int i, long long v, vector<long long> &t) {
    int n = static_cast<int>((t.size() + 1) / 2);
    t[n - 1 + i] += v;
    if (n - 1 + i != 0) {
        update(parent(n - 1 + i), t);
    }
}

bool is_ancestor(int a, int b, vector<int> &tin, vector<int> &tout) {
    return tin[a] <= tin[b] && tout[a] >= tout[b];
}

int lca(int a, int b, vector<int> &tin, vector<int> &tout, vector<vector<int>> &st) {
    if (is_ancestor(a, b, tin, tout)) {
        return a;
    }
    if (is_ancestor(b, a, tin, tout)) {
        return b;
    }
    for (size_t i = st[0].size(); i-- > 0;) {
        if (!is_ancestor(st[a][i], b, tin, tout)) {
            a = st[a][i];
        }
    }
    return st[a][0];
}

int pow2(int n) {
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n++;
    return n;
}

int timer = 0;

void dfs(int u, vector<int> &tin, vector<int> &tout, int parent, vector<vector<int>> &g,
         vector<vector<int>> &st, vector<int> &p) {
    tin[u] = timer;
    timer++;
    st[u][0] = parent;
    p[u] = parent;
    for (size_t i = 1; i < st[0].size(); i++) {
        st[u][i] = st[st[u][i - 1]][i - 1];
    }
    for (int v : g[u]) {
        if (v != parent) {
            dfs(v, tin, tout, u, g, st, p);
        }
    }
    tout[u] = timer - 1;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int nReal, m;
    cin >> nReal;
    vector<int> tin(nReal), tout(nReal), p(nReal);
    vector<vector<int>> g(nReal);
    for (int i = 0; i < nReal - 1; ++i) {
        int u, v;
        cin >> u >> v;
        g[u - 1].push_back(v - 1);
        g[v - 1].push_back(u - 1);
    }
    int lo = 1;
    while ((1 << lo) <= nReal) {
        lo++;
    }
    vector<vector<int>> st(nReal, vector<int>(lo + 1));
    dfs(0, tin, tout, 0, g, st, p);

    int doSize = pow2(tout[0] - tin[0] + 1);
    vector<long long> t(static_cast<unsigned long long>(2 * doSize - 1));
    for (int i = 0; i < doSize; i++) {
        t[doSize - 1 + i] = 0;
    }
    for (int i = doSize - 2; i >= 0; i--) {
        t[i] = t[left(i)] + t[right(i)];
    }
    cin >> m;
    string cmd;
    for (int i = 0; i < m; i++) {
        cin >> cmd;
        if (cmd == "+") {
            int v, u, x;
            cin >> v >> u >> x;
            int l = lca(v - 1, u - 1, tin, tout, st);
            add_v(tin[v - 1], x, t);
            add_v(tin[u - 1], x, t);
            add_v(tin[l], -x, t);
            if (l != 0) {
                add_v(tin[p[l]], -x, t);
            }
        } else {
            int v;
            cin >> v;
            long long res = query(tin[v - 1], tout[v - 1] + 1, t);
            cout << res << "\n";
        }
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
