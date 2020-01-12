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

struct Entry {
    int add;
    int max;
    int index;

    Entry() {
        add = 0;
        max = 0;
        index = -1;
    }

    explicit Entry(int add_, int max_, int index_) : add(add_), max(max_), index(index_) {}
};

void push(int node, vector<Entry> &t) {
    t[left(node)].add += t[node].add;
    t[right(node)].add += t[node].add;
    t[node].add = 0;
}

int get_max_node(int node, vector<Entry> const &t) {
    return t[node].max + t[node].add;
}

void add(int node, int a, int b, int l, int r, int v, vector<Entry> &t) {
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
    int lefttt = get_max_node(left(node), t);
    int righttt = get_max_node(right(node), t);
    if (lefttt > righttt) {
        t[node].max = lefttt;
        t[node].index = t[left(node)].index;
    } else {
        t[node].max = righttt;
        t[node].index = t[right(node)].index;
    }
}

void add(int l, int r, int v, vector<Entry> &t) {
    add(0, l, r, 0, static_cast<int>((t.size() + 1) / 2), v, t);
}

Entry get_max(int node, int a, int b, int l, int r, vector<Entry> &t) {
    if (l >= b || r <= a) {
        return Entry(0, -INF, -1);
    }
    if (l >= a && r <= b) {
        return Entry(0, get_max_node(node, t), t[node].index);
    }
    push(node, t);
    int m = (l + r) / 2;
    Entry ans;
    Entry leftt = get_max(left(node), a, b, l, m, t);
    Entry rightt = get_max(right(node), a, b, m, r, t);
    if (leftt.max > rightt.max) {
        ans = leftt;
    } else {
        ans = rightt;
    }
    int lefttt = get_max_node(left(node), t);
    int righttt = get_max_node(right(node), t);
    if (lefttt > righttt) {
        t[node].max = lefttt;
        t[node].index = t[left(node)].index;
    } else {
        t[node].max = righttt;
        t[node].index = t[right(node)].index;
    }
    return ans;
}

Entry get_max(int a, int b, vector<Entry> &t) {
    return get_max(0, a, b, 0, static_cast<int>((t.size() + 1) / 2), t);
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

struct Event {
    int x;
    int y1;
    int y2;
    int type;

    Event(int x_, int y1_, int y2_, int type_) : x(x_), y1(y1_), y2(y2_), type(type_) {}
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
//    freopen("b.in", "r", stdin);
    int rects;
    cin >> rects;
    vector<Event> a;
    int minx, maxx, miny, maxy;
    minx = static_cast<int>(1e6 + 100);
    maxx = static_cast<int>(-1e6 - 100);
    miny = static_cast<int>(1e6 + 100);
    maxy = static_cast<int>(-1e6 - 100);
    int x1, y1, x2, y2;
    for (int i = 0; i < rects; ++i) {
        cin >> x1 >> y1 >> x2 >> y2;
        minx = min(minx, x1);
        maxx = max(maxx, x2);
        miny = min(miny, y1);
        maxy = max(maxy, y2);
        a.emplace_back(x1, y1, y2, 0);
        a.emplace_back(x2, y1, y2, 2);
    }
    for (auto &event : a) {
        event.y1 -= miny;
        event.y2 -= miny;
    }
    for (int i = minx; i <= maxx; ++i) {
        a.emplace_back(i, 0, 0, 1);
    }
    auto sortRuleLambda = [](const Event &e1, const Event &e2) {
        if (e1.x < e2.x) {
            return true;
        }
        if (e1.x > e2.x) {
            return false;
        }
        return e1.type < e2.type;
    };
    std::sort(a.begin(), a.end(), sortRuleLambda);
    int ys = maxy - miny + 1;
    int n = pow2(ys);

    vector<Entry> t(static_cast<unsigned int>(2 * n - 1));
    for (int i = 0; i < n; i++) {
        t[n - 1 + i].index = i;
    }
    for (int i = n - 2; i >= 0; i--) {
        t[i].index = t[left(i)].index;
    }
    int resMax = -INF;
    int resX = 0;
    int resY = 0;
    for (auto &event : a) {
        if (event.type == 0) {
            add(event.y1, event.y2 + 1, 1, t);
        } else if (event.type == 2) {
            add(event.y1, event.y2 + 1, -1, t);
        } else {
            Entry cur_max = get_max(0, ys, t);
            if (resMax < cur_max.max) {
                resMax = cur_max.max;
                resX = event.x;
                resY = cur_max.index;
            }
        }
    }
    cout << resMax << "\n" << resX << " " << resY + miny << "\n";
    return 0;
}
