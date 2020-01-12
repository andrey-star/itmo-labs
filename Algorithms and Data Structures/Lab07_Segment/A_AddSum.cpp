#include <bits/stdc++.h>

using namespace std;
typedef uint32_t ui;
size_t n = 16777216;
vector<ui> a(n);

ui ar, br;
ui cur = 0;

ui nextRand() {
    cur = cur * ar + br;
    return cur >> 8;
}

void add(size_t l, size_t r, ui v) {
    a[r] += v;
    if (l > 0) {
        a[l - 1] -= v;
    }
}

ui sumt(size_t l, size_t r) {
    ui lval = l > 0 ? a[l - 1] : 0;
    return a[r] - lval;
}

int main() {
    size_t m, q;
    cin >> m >> q;
    cin >> ar >> br;
    for (size_t i = 0; i < m; ++i) {
        ui addt = nextRand();
        size_t l = nextRand();
        size_t r = nextRand();
        if (l > r) {
            add(r, l, addt);
        } else {
            add(l, r, addt);
        }
    }
    for (size_t i = n - 1; i-- > 0;) {
        a[i] += a[i + 1];
    }
    for (size_t i = 1; i < n; i++) {
        a[i] += a[i - 1];
    }
    ui sum = 0;
    for (size_t i = 0; i < q; ++i) {
        size_t l = nextRand();
        size_t r = nextRand();
        if (l > r) {
            sum += sumt(r, l);
        } else {
            sum += sumt(l, r);
        }
    }
    cout << sum << "\n";
    return 0;
}
