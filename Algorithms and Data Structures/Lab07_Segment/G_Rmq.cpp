#include <bits/stdc++.h>

using namespace std;

mt19937_64 rng((unsigned int) chrono::steady_clock::now().time_since_epoch().count());

template <class T>
T rnd(T l, T r) {
    return uniform_int_distribution<T>(l, r)(rng);
}

int left(int i) {
    return 2 * i + 1;
}

int right(int i) {
    return left(i) + 1;
}

int parent(int i) {
    return (i - 1) / 2;
}

long long INF = static_cast<long long>(1e18 + 128);

struct Entry {
    long long add;
    long long min;
    long long set;

    Entry() {
        add = 0;
        min = 0;
        set = INF;
    }

    explicit Entry(long long add_, long long min_) : add(add_), min(min_), set(INF) {}
};

void push(int node, vector<Entry> &t) {
    if (t[node].set != INF) {
        t[left(node)].set = t[node].set;
        t[right(node)].set = t[node].set;
        t[node].set = INF;
        t[left(node)].add = t[node].add;
        t[right(node)].add = t[node].add;
        t[node].add = 0;
    } else {
        t[left(node)].add += t[node].add;
        t[right(node)].add += t[node].add;
        t[node].add = 0;
    }
}

long long get_min_node(int node, vector<Entry> const &t) {
    return ((t[node].set == INF) ? t[node].min : t[node].set) + t[node].add;
}

void add(int node, int a, int b, int l, int r, long long v, vector<Entry> &t) {
    if (l >= b || r <= a) {
        return;
    }
    if (l >= a && r <= b) {
        t[node].add += v;
        return;
    }
    push(node, t);
    int m = (l + r) / 2;
    add(left(node), a, b, l, m, v, t);
    add(right(node), a, b, m, r, v, t);
    t[node].min = min(get_min_node(left(node), t), get_min_node(right(node), t));
}

void add(int l, int r, long long v, vector<Entry> &t) {
    add(0, l, r, 0, static_cast<int>((t.size() + 1) / 2), v, t);
}

void sett(int node, int a, int b, int l, int r, long long v, vector<Entry> &t) {
    if (l >= b || r <= a) {
        return;
    }
    if (l >= a && r <= b) {
        t[node].set = v;
        t[node].add = 0;
        return;
    }
    push(node, t);
    int m = (l + r) / 2;
    sett(left(node), a, b, l, m, v, t);
    sett(right(node), a, b, m, r, v, t);
    t[node].min = min(get_min_node(left(node), t), get_min_node(right(node), t));
}

void sett(int l, int r, long long v, vector<Entry> &t) {
    sett(0, l, r, 0, static_cast<int>((t.size() + 1) / 2), v, t);
}

long long get_min(int node, int a, int b, int l, int r, vector<Entry> &t) {
    if (l >= b || r <= a) {
        return INF;
    }
    if (l >= a && r <= b) {
        return get_min_node(node, t);
    }
    push(node, t);
    int m = (l + r) / 2;
    long long ans = min(get_min(left(node), a, b, l, m, t), get_min(right(node), a, b, m, r, t));
    t[node].min = min(get_min_node(left(node), t), get_min_node(right(node), t));
    return ans;
}

long long get_min(int a, int b, vector<Entry> &t) {
    return get_min(0, a, b, 0, static_cast<int>((t.size() + 1) / 2), t);
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

long long get_min(int l, int r, vector<long long> &a) {
    long long mint = INF;
    for (int i = l; i < r; ++i) {
        mint = min(mint, a[i]);
    }
    return mint;
}

void sett(int l, int r, long long v, vector<long long> &a) {
    for (int i = l; i < r; ++i) {
        a[i] = v;
    }
}

void add(int l, int r, long long v, vector<long long> &a) {
    for (int i = l; i < r; ++i) {
        a[i] += v;
    }
}

void print(vector<Entry> &t) {
    int n = static_cast<int>((t.size() + 1) / 2);
    for (int i = 0; i < n; ++i) {
        cout << get_min(i, i + 1, t) << " ";
    }
    cout << "\n";
}

void gen() {
    freopen("b.in", "w", stdout);
    int n = static_cast<int>(1e2);
    cout << n << "\n";
    for (int i = 0; i < n; ++i) {
        cout << rnd(-n, n) << " ";
    }
    cout << "\n";
    for (int j = 0; j < n; ++j) {
        int l = rnd(0, n - 1) + 1;
        int r = rnd(0, n - 1) + 1;
        if (l > r) {
            int temp = l;
            l = r;
            r = temp;
        }
        int type = rnd(0, 2);
        if (type == 1 || type == 2) {
            if (type == 1) {
                cout << "add ";
            } else {
                cout << "set ";
            }
            cout << l << " " << r << " " << rnd(-n, n) << "\n";
        } else {
            cout << "min ";
            cout << l << " " << r << "\n";
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    gen();
//    freopen("b.in", "r", stdin);
    int nReal;
    cin >> nReal;
    int n = pow2(nReal);
    vector<long long> a(static_cast<unsigned long long>(n));
//    vector<long long> res(static_cast<unsigned long long int>(nReal));
    for (int i = 0; i < n; i++) {
        if (i < nReal) {
            int v;
            cin >> v;
            a[i] = v;
//            res[i] = v;
        } else {
            a[i] = INF;
        }
    }
    vector<Entry> t(static_cast<unsigned long long>(2 * n - 1));
    for (int i = 0; i < n; i++) {
        t[n - 1 + i] = Entry(0, a[i]);
    }
    for (int i = n - 2; i >= 0; i--) {
        t[i].min = min(t[left(i)].min, t[right(i)].min);
    }
    string cmd;
    int num = 0;
    while (cin >> cmd) {
        num++;
        int A, B;
        cin >> A >> B;
        if (cmd == "min") {
            cout << get_min(A - 1, B, t) << "\n";
        } else if (cmd == "set") {
            long long V;
            cin >> V;
            sett(A - 1, B, V, t);
//            sett(A - 1, B, V, res);
        } else {
            long long V;
            cin >> V;
            add(A - 1, B, V, t);
//            add(A - 1, B, V, res);
        }
    }
    return 0;
}
