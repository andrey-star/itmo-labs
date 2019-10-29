#include <bits/stdc++.h>

using namespace std;

int min(int l, int r, vector<vector<int>> const &table, vector<int> const &log2) {
    if (l > r) {
        int temp = l;
        l = r;
        r = temp;
    }
    int j = log2[r - l + 1];
    return min(table[l][j], table[r - (1 << j) + 1][j]);
}

int main() {
    int n, m, ar, u, v;
    cin >> n >> m >> ar >> u >> v;
    vector<int> a(static_cast<unsigned long long int>(n));
    a[0] = ar;
    for (int i = 1; i < n; i++) {
        a[i] = (int) (23L * a[i - 1] + 21563) % 16714589;
    }
    vector<int> log2(static_cast<unsigned long long int>(n + 1));
    log2[1] = 0;
    for (int i = 2; i <= n; i++) {
        log2[i] = log2[i / 2] + 1;
    }
    int maxPow = log2[n];
    vector<vector<int>> table(static_cast<unsigned long long int>(n), vector<int>(
            static_cast<unsigned long long int>(maxPow + 1)));
    for (int i = 0; i < n; i++) {
        table[i][0] = a[i];
    }
    for (int i = n - 1; i >= 0; i--) {
        for (int j = 1; j < maxPow + 1; j++) {
            table[i][j] = min(table[i][j - 1], table[min(i + (1 << (j - 1)), n - 1)][j - 1]);
        }
    }
    int ans = min(u - 1, v - 1, table, log2);
    for (int i = 1; i < m; i++) {
        u = (int) ((17L * u + 751 + ans + 2 * i) % n) + 1;
        v = (int) ((13L * v + 593 + ans + 5 * i) % n) + 1;
        ans = min(u - 1, v - 1, table, log2);
    }
    cout << u << " " << v << " " << ans << "\n";
    return 0;
}
