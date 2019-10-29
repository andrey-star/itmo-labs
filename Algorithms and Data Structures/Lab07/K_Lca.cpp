#include <bits/stdc++.h>

using namespace std;

void add(int a, int b, vector<vector<int>> &st, vector<int> &p, vector<int> &d) {
    size_t n = max<size_t>(p.size(), static_cast<size_t>(b + 1));
    p.resize(n);
    d.resize(n);
    p[b] = a;
    d[b] = d[a] + 1;
    st.resize(n);
    st[b] = {p[b]};
    int k = 1;
    while (true) {
        if (st[st[b][k - 1]].size() >= static_cast<size_t>(k)) {
            st[b].push_back(st[st[b][k - 1]][k - 1]);
        } else {
            st[b].push_back(0);
        }
        if (st[b][k] == 0) {
            break;
        }
        k++;
    }
}

int lca(int a, int b, vector<vector<int>> &st, vector<int> &p, vector<int> &d) {
    if (d[a] < d[b]) {
        swap(a, b);
    }
    int dif = d[a] - d[b];
    for (int i = 0;; i++) {
        if ((dif >> i) == 0) {
            break;
        }
        int cur = ((dif >> i) & 1);
        if (cur == 1) {
            a = st[a][i];
        }
    }
    if (a == b) {
        return a;
    }
    for (int i = static_cast<int>(st[a].size() - 1); i >= 0; i--) {
        if (st[a][i] != st[b][i]) {
            a = st[a][i];
            b = st[b][i];
        }
    }
    return p[a];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int q;
    cin >> q;
    vector<int> p, d;
    p.push_back(0);
    d.push_back(0);
    vector<vector<int>> st = {{0}};
    string cmd;
    int A, B;
    for (int i = 0; i < q; i++) {
        cin >> cmd;
        cin >> A >> B;
        if (cmd == "GET") {
            cout << lca(A - 1, B - 1, st, p, d) + 1 << "\n";
        } else {
            add(A - 1, B - 1, st, p, d);
        }
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
