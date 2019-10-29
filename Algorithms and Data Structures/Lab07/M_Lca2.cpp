#include <bits/stdc++.h>

using namespace std;

pair<int, int> rmq(int l, int r, vector<vector<pair<int, int>>> const &table,
        vector<int> const &log_2) {
    if (l > r) {
        swap(l, r);
    }
    int j = log_2[r - l + 1];
    return min(table[l][j], table[r - (1 << j) + 1][j]);
}

int lca(int a, int b, vector<int> const &first_occ,
        vector<vector<pair<int, int>>> const &table, vector<int> const &log_2) {
    if (first_occ[a] > first_occ[b]) {
        swap(a, b);
    }
    return rmq(first_occ[a], first_occ[b], table, log_2).second;
}

void dfs(vector<vector<int>> const &g, int u, int depth,
         vector<pair<int, int>> &d, vector<int> &first_occ) {
    first_occ[u] = static_cast<int>(d.size());
    d.emplace_back(depth, u);
    for (int v : g[u]) {
        dfs(g, v, depth + 1, d, first_occ);
        d.emplace_back(depth, u);
    }
}

vector<vector<int>> test(int n) {
    vector<vector<int>> res(static_cast<unsigned long long int>(n));
    for (int i = 1; i < n; i++) {
        res[0].push_back(i);
    }
    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int gSize, m;
    cin >> gSize >> m;

    vector<vector<int>> g(static_cast<unsigned long long>(gSize));
    for (int i = 1; i < gSize; i++) {
        int p_of_i;
        cin >> p_of_i;
        g[p_of_i].push_back(i);
    }
//    auto g = test(gSize);
    vector<pair<int, int>> d;
    vector<int> first_occ(static_cast<unsigned long long>(gSize));
    dfs(g, 0, 0, d, first_occ);

    // rmq sparse table
    int n = static_cast<int>(d.size());
    vector<int> log_2(static_cast<unsigned long long>(n + 1));
    log_2[1] = 0;
    for (int i = 2; i <= n; i++) {
        log_2[i] = log_2[i / 2] + 1;
    }
    int maxPow = log_2[n];
    vector<vector<pair<int, int>>> table(static_cast<unsigned long long>(n), vector<pair<int, int>>(
            static_cast<unsigned long long int>(maxPow + 1)));
    for (int i = 0; i < n; i++) {
        table[i][0] = d[i];
    }
    for (int i = n - 1; i >= 0; i--) {
        for (int j = 1; j < maxPow + 1; j++) {
            table[i][j] = min(table[i][j - 1], table[min(i + (1 << (j - 1)), n - 1)][j - 1]);
        }
    }

    int a1, a2, x, y, z;
    cin >> a1 >> a2 >> x >> y >> z;
    int v = lca(a1, a2, first_occ, table, log_2);
    long long sum = 0;
    sum += v;
    for (int i = 0; i < m - 1; i++) {
        a1 = static_cast<int>(((long long) x * a1 + (long long) y * a2 + z) % gSize);
        a2 = static_cast<int>(((long long) x * a2 + (long long) y * a1 + z) % gSize);
        v = lca((a1 + v) % gSize, a2, first_occ, table, log_2);
        sum += v;
    }
    cout << sum << "\n";
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
