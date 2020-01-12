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

int INF = static_cast<int>(1e9 + 128);

void update(int i, vector<int> &t) {
    if (t[left(i)] != INF) {
        t[i] = t[left(i)];
    } else {
        t[i] = t[right(i)];
    }
    if (i == 0) {
        return;
    }
    update(parent(i), t);
}

void sett(int i, int v, vector<int> &t) {
    int n = static_cast<int>((t.size() + 1) / 2);
    t[n - 1 + i] = v;
    if (n - 1 + i != 0) {
        update(parent(n - 1 + i), t);
    }
}

void take(int i, vector<int> &t) {
    if (i != INF) {
        sett(i, INF, t);
    }
}

void exit(int i, vector<int> &t) {
    sett(i, i, t);
}

int enter(int node, int a, int b, int l, int r, vector<int> &t) {
    if (l >= b || r <= a) {
        return INF;
    }
    if (l >= a && r <= b) {
        return t[node];
    }
    int m = (l + r) / 2;
    return min(enter(left(node), a, b, l, m, t), enter(right(node), a, b, m, r, t));
}

int enter(int i, vector<int> &t) {
    int n = static_cast<int>((t.size() + 1) / 2);
    int right = enter(0, i, n, 0, n, t);
    int res = (right != INF ? right : enter(0, 0, n, 0, n, t));
    take(res, t);
    return res;
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

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int nReal, q;
    cin >> nReal >> q;
    int n = pow2(nReal);
    vector<int> t(static_cast<unsigned long long>(2 * n - 1));
    for (int i = 0; i < n; i++) {
        if (i < nReal) {
            t[n - 1 + i] = i;
        } else {
            t[n - 1 + i] = INF;
        }
    }
    for (int i = n - 2; i >= 0; i--) {
        t[i] = min(t[left(i)], t[right(i)]);
    }
    for (int i = 0; i < q; i++) {
        string cmd;
        cin >> cmd;
        if (cmd == "enter") {
            int A;
            cin >> A;
            cout << enter(A - 1, t) + 1 << "\n";
        } else {
            int A;
            cin >> A;
            exit(A - 1, t);
        }
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
