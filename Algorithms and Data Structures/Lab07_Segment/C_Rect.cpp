#include <bits/stdc++.h>

using namespace std;

int A, B, V;
int mod = (int) 1e9 + 7;
int n, m;

int nextV() {
    V = (int) (((long long) A * V + B) % mod);
    return V;
}

struct Rect {
    int x1, y1, x2, y2;

    Rect() = default;

    Rect(int x1_, int y1_, int x2_, int y2_) : x1(x1_), y1(y1_), x2(x2_), y2(y2_) {}

    int area() {
        int x = max(x2 - x1, 0);
        int y = max(y2 - y1, 0);
        return (int) (((long long) x * y) % mod);
    }
};

Rect intersect(Rect f, Rect s) {
    int x1 = max(f.x1, s.x1);
    int y1 = max(f.y1, s.y1);
    int x2 = min(f.x2, s.x2);
    int y2 = min(f.y2, s.y2);
    return {x1, y1, x2, y2};
}

Rect st[127][127][7][7];

int query(int r1, int c1, int r2, int c2, vector<int> const &log2j) {
    if (r1 > r2) {
        int temp = r1;
        r1 = r2;
        r2 = temp;
    }
    if (c1 > c2) {
        int temp = c1;
        c1 = c2;
        c2 = temp;
    }
    int logx = log2j[r2 - r1 + 1];
    int logy = log2j[c2 - c1 + 1];
    Rect one = st[r1][c1][logx][logy];
    Rect two = st[r2 - (1 << logx) + 1][c1][logx][logy];
    Rect thr = st[r1][c2 - (1 << logy) + 1][logx][logy];
    Rect four = st[r2 - (1 << logx) + 1][c2 - (1 << logy) + 1][logx][logy];
    Rect res = intersect(intersect(one, two), intersect(thr, four));
    return res.area();
}

int main() {
//    freopen("b.in", "r", stdin);
    cin >> n >> m;
    Rect table[127][127];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            int x1, y1, x2, y2;
            cin >> x1 >> y1 >> x2 >> y2;
            if (x1 > x2) {
                int temp = x1;
                x1 = x2;
                x2 = temp;
            }
            if (y1 > y2) {
                int temp = y1;
                y1 = y2;
                y2 = temp;
            }
            table[i][j] = Rect(x1, y1, x2, y2);
        }
    }
    int Q;
    cin >> Q >> A >> B >> V;
    vector<int> log2(static_cast<unsigned long long int>(max(n, m) + 1));
    log2[1] = 0;
    for (int i = 2; i < max(n, m) + 1; i++) {
        log2[i] = log2[i / 2] + 1;
    }
    int maxPowN = log2[n];
    int maxPowM = log2[m];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            st[i][j][0][0] = table[i][j];
        }
    }
    for (int i = n - 1; i >= 0; i--) {
        for (int j = m - 1; j >= 0; j--) {
            for (int k = 1; k < maxPowN + 1; k++) {
                Rect f = st[i][j][k - 1][0];
                Rect s = st[min(i + (1 << (k - 1)), n - 1)][j][k - 1][0];
                st[i][j][k][0] = intersect(f, s);
            }
        }
    }
    for (int i = n - 1; i >= 0; i--) {
        for (int j = m - 1; j >= 0; j--) {
            for (int k = 0; k < maxPowN + 1; k++) {
                for (int l = 1; l < maxPowM + 1; l++) {
                    Rect f = st[i][j][k][l - 1];
                    Rect s = st[i][min(j + (1 << (l - 1)), m - 1)][k][l - 1];
                    st[i][j][k][l] = intersect(f, s);
                }
            }
        }
    }
    int ans = 0;
    for (int i = 0; i < Q; i++) {
        int r1 = nextV() % n;
        int c1 = nextV() % m;
        int r2 = nextV() % n;
        int c2 = nextV() % m;
        int g = query(r1, c1, r2, c2, log2);
        ans += g;
        ans %= mod;
    }
    cout << ans << "\n";
    return 0;
}
