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

int r;

struct Matrix {
    int one_one, one_two, two_one, two_two;

    Matrix() = default;

    Matrix(int one_one_, int one_two_, int two_one_, int two_two_) :
            one_one(one_one_), one_two(one_two_),
            two_one(two_one_), two_two(two_two_) {}
};

Matrix multiply(Matrix f, Matrix s) {
    return {(f.one_one * s.one_one + f.one_two * s.two_one) % r,
            (f.one_one * s.one_two + f.one_two * s.two_two) % r,
            (f.two_one * s.one_one + f.two_two * s.two_one) % r,
            (f.two_one * s.one_two + f.two_two * s.two_two) % r};
}

Matrix query(int node, int a, int b, int l, int r, vector<Matrix> const &t) {
    if (l >= b || r <= a) {
        return {1, 0, 0, 1};
    }
    if (l >= a && r <= b) {
        return t[node];
    }
    int m = (l + r) / 2;
    return multiply(query(left(node), a, b, l, m, t), query(right(node), a, b, m, r, t));
}

Matrix query(int a, int b, vector<Matrix> const &t) {
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

string to_string(Matrix m) {
    return to_string(m.one_one) + " "
           + to_string(m.one_two) + "\n"
           + to_string(m.two_one) + " "
           + to_string(m.two_two);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int nReal, m;
    cin >> r >> nReal >> m;
    int n = nReal;
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n++;
    vector<Matrix> a(static_cast<unsigned long long>(n));
    for (int i = 0; i < n; i++) {
        if (i < nReal) {
            int one_one, one_two, two_one, two_two;
            cin >> one_one >> one_two >> two_one >> two_two;
            a[i] = Matrix(one_one, one_two, two_one, two_two);
        } else {
            a[i] = Matrix(1, 0, 0, 1);
        }
    }
    vector<Matrix> t(static_cast<unsigned long long>(2 * n - 1));
    for (int i = 0; i < n; i++) {
        t[n - 1 + i] = a[i];
    }
    for (int i = n - 2; i >= 0; i--) {
        t[i] = multiply(t[left(i)], t[right(i)]);
    }
    int A, B;
    while (cin >> A) {
        cin >> B;
        cout << to_string(query(A - 1, B, t)) << "\n\n";
    }
//    cerr << 1.0 * clock() / CLOCKS_PER_SEC;
    return 0;
}
