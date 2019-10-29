#include <bits/stdc++.h>

using namespace std;

int inf = static_cast<int>(1e6 + 100);

int timer = 0;

const int nn = (int) 2e5 + 10;
const int log_nn = 20;

int tin[nn], tout[nn], w[nn];

vector<int> g[nn];

pair<int, int> st[nn][log_nn];

void dfs(int u, int parent, int st_size) {
    tin[u] = timer;
    timer++;
    st[u][0] = {parent, w[u]};
    for (int i = 1; i < st_size; i++) {
        auto fir = st[u][i - 1];
        auto sec = st[fir.first][i - 1];
        st[u][i] = {sec.first, min(fir.second, sec.second)};
    }
    for (int v : g[u]) {
        if (v != parent) {
            dfs(v, u, st_size);
        }
    }
    tout[u] = timer - 1;
}

bool is_ancestor(int a, int b) {
    return tin[a] <= tin[b] && tout[a] >= tout[b];
}

int minim(int a, int b, int st_size) {
    int mint = inf;
    int lac = a;
    if (!is_ancestor(a, b)) {
        for (int i = st_size - 1; i >= 0; i--) {
            if (!is_ancestor(st[a][i].first, b)) {
                mint = min(mint, st[a][i].second);
                a = st[a][i].first;
            }
        }
        mint = min(mint, st[a][0].second);
        lac = st[a][0].first;
    }
    if (lac != b) {
        for (int i = st_size - 1; i >= 0; i--) {
            if (!is_ancestor(st[b][i].first, lac)) {
                mint = min(mint, st[b][i].second);
                b = st[b][i].first;
            }
        }
        mint = min(mint, st[b][0].second);
    }
    return mint;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int nReal, q;
    cin >> nReal;
    int log_n = 1;
    while ((1 << log_n) <= nReal) {
        log_n++;
    }
    for (int i = 1; i < nReal; ++i) {
        int par, weight;
        cin >> par >> weight;
        par--;
        g[par].push_back(i);
        g[i].push_back(par);
        w[i] = weight;
    }
    dfs(0, 0, log_n + 1);

    cin >> q;
    int A, B;
    for (int i = 0; i < q; i++) {
        cin >> A >> B;
        cout << minim(A - 1, B - 1, log_n + 1) << "\n";
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
