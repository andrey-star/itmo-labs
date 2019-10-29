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

void sett(int i, long long v, vector<long long> &t) {
    int n = static_cast<int>((t.size() + 1) / 2);
    t[n - 1 + i] = v;
    if (n - 1 + i != 0) {
        update(parent(n - 1 + i), t);
    }
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int nReal;
    cin >> nReal;
    int n = nReal;
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n++;
    vector<long long> a(static_cast<unsigned long long>(n));
    for (int i = 0; i < n; i++) {
        if (i < nReal) {
            cin >> a[i];
        } else {
            a[i] = 0;
        }
    }
    vector<long long> t(static_cast<unsigned long long>(2 * n - 1));
    for (int i = 0; i < n; i++) {
        t[n - 1 + i] = a[i];
    }
    for (int i = n - 2; i >= 0; i--) {
        t[i] = t[left(i)] + t[right(i)];
    }
    string cmd;
    while (cin >> cmd) {
        if (cmd == "sum") {
            int A, B;
            cin >> A >> B;
            cout << query(A - 1, B, t) << "\n";
        } else {
            int A;
            long long B;
            cin >> A >> B;
            sett(A - 1, B, t);
        }
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
