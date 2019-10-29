#pragma once


#include <iterator>

template<typename T>
struct list {

    struct node {

        node() : prev(this), next(this) {}

        node(node *prev, node *next) : prev(prev), next(next) {}

        node *prev;
        node *next;
    };

    struct node_val : node {
        node_val(node *prev, node *next, T const &value) : node(prev, next), value(value) {}

    private:
        T value;

        friend struct list;
    };

public:
    list() = default;

    list(list const &other) : list() {
        for (T const &i : other) {
            push_back(i);
        }
    }

    list &operator=(list const &rhs) {
        list copy = rhs;
        swap(*this, copy);
        return *this;
    }

    void clear() {
        if ((fake.next == &fake) && (fake.prev = &fake)) {
            return;
        }
        for (node *i = fake.next; i != &fake;) {
            node *j = i->next;
            delete static_cast<node_val *>(i);
            i = j;
        }
        fake.next = &fake;
        fake.prev = &fake;
    }

    bool empty() {
        return fake.next == &fake;
    }

    ~list() {
        clear();
    }

    void push_back(T const &val) {
        auto *new_node = new node_val(fake.prev, &fake, val);
        new_node->prev->next = new_node;
        new_node->next->prev = new_node;
    }

    void push_front(T const &val) {
        auto *new_node = new node_val(&fake, fake.next, val);
        new_node->prev->next = new_node;
        new_node->next->prev = new_node;
    }

    void pop_back() {
        node *del = fake.prev;
        fake.prev->prev->next = &fake;
        fake.prev = fake.prev->prev;
        delete static_cast<node_val *>(del);
    }

    void pop_front() {
        node *del = fake.next;
        fake.next->next->prev = &fake;
        fake.next = fake.next->next;
        delete static_cast<node_val *>(del);
    }

    template<typename V>
    struct list_iterator : std::iterator<std::bidirectional_iterator_tag, V> {

        list_iterator() = default;

        list_iterator(list_iterator<T> const &other) : ptr(other.ptr) {}

        list_iterator &operator=(list_iterator const &) = default;

        V &operator*() const {
            return static_cast<node_val *>(ptr)->value;
        }

        V *operator->() const {
            return &static_cast<node_val *>(ptr)->value;
        }

        list_iterator operator++() {
            ptr = ptr->next;
            return *this;
        }

        const list_iterator operator++(int) {
            list_iterator copy = *this;
            ++*this;
            return copy;
        }

        list_iterator operator--() {
            ptr = ptr->prev;
            return *this;
        }

        list_iterator operator--(int) {
            list_iterator copy = *this;
            --*this;
            return copy;
        }

        friend bool operator==(list_iterator const &a, list_iterator const &b) {
            return a.ptr == b.ptr;
        }

        friend bool operator!=(list_iterator const &a, list_iterator const &b) {
            return a.ptr != b.ptr;
        }

    private:
        node *ptr;

        explicit list_iterator(node *p) : ptr(p) {};

        friend struct list;
    };

    using iterator = list_iterator<T>;
    using const_iterator = list_iterator<T const>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() {
        return iterator(fake.next);
    }

    iterator end() {
        return iterator(&fake);
    }

    const_iterator begin() const {
        return const_iterator(fake.next);
    }

    const_iterator end() const {
        return const_iterator(const_cast<node *>(&fake));
    }

    reverse_iterator rbegin() {
        return reverse_iterator(end());
    }

    reverse_iterator rend() {
        return reverse_iterator(begin());
    }

    const_reverse_iterator rbegin() const {
        return const_reverse_iterator(end());
    }

    const_reverse_iterator rend() const {
        return const_reverse_iterator(begin());
    }

    iterator insert(const_iterator pos, T const &val) {
        auto *new_node = new node_val(pos.ptr->prev, pos.ptr, val);
        new_node->prev->next = new_node;
        new_node->next->prev = new_node;

        return iterator(new_node);
    }

    iterator erase(const_iterator pos) {
        node *del = pos.ptr;
        pos.ptr->prev->next = pos.ptr->next;
        pos.ptr->next->prev = pos.ptr->prev;
        node *res = pos.ptr->next;
        delete static_cast<node_val *>(del);
        return iterator(res);
    }

    T &back() {
        return static_cast<node_val *>(fake.prev)->value;
    }

    T const &back() const {
        return static_cast<node_val *>(fake.prev)->value;
    }

    T &front() {
        return static_cast<node_val *>(fake.next)->value;
    }

    T const &front() const {
        return static_cast<node_val *>(fake.next)->value;
    }

    void splice(const_iterator pos, list &other, const_iterator first, const_iterator last) {
        if (first == last) {
            return;
        }
        node *before_first = first.ptr->prev;
        first.ptr->prev = pos.ptr->prev;
        pos.ptr->prev->next = first.ptr;

        last.ptr->prev->next = pos.ptr;
        pos.ptr->prev = last.ptr->prev;

        last.ptr->prev = before_first;
        before_first->next = last.ptr;
    }

    template<typename V>
    friend void swap(list<V> &a, list<V> &b);

private:
    node fake;

};

template<typename T>
void swap(list<T> &a, list<T> &b) {
    if (a.empty() && b.empty()) {
        return;
    }
    if (a.empty() && !b.empty()) {
        a.fake = b.fake;
        a.fake.next->prev = &a.fake;
        a.fake.prev->next = &a.fake;
        b.fake.next = &b.fake;
        b.fake.prev = &b.fake;
        return;
    }
    if (!a.empty() && b.empty()) {
        swap(b, a);
        return;
    }
    std::swap(a.fake, b.fake);
    a.fake.next->prev = &a.fake;
    a.fake.prev->next = &a.fake;
    b.fake.next->prev = &b.fake;
    b.fake.prev->next = &b.fake;

}
