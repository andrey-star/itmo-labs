#pragma once

#include <iterator>
#include <iostream>
#include <cassert>

template<typename T>
struct vector {

    vector() noexcept;

    vector(vector const &other);

    vector &operator=(vector const &other);

    template<typename InputIterator>
    vector(InputIterator first, InputIterator last);

    ~vector();

    template<typename InputIterator>
    void assign(InputIterator first, InputIterator last);

    using value_type = T;
    typedef T *iterator;
    typedef const T *const_iterator;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    T &operator[](size_t index);

    T const &operator[](size_t index) const;

    T &front() noexcept;

    T const &front() const noexcept;

    T &back() noexcept;

    T const &back() const noexcept;

    void push_back(T const &value);

    void pop_back();

    iterator begin();

    const_iterator begin() const noexcept;

    iterator end();

    const_iterator end() const noexcept;

    reverse_iterator rbegin();

    const_reverse_iterator rbegin() const noexcept;

    reverse_iterator rend();

    const_reverse_iterator rend() const noexcept;

    T *data();

    T const *data() const noexcept;

    bool empty() const noexcept;

    size_t size() const noexcept;

    size_t capacity() const noexcept;

    void clear() noexcept;

    void reserve(size_t n);

    void resize(size_t n, T const &val);

    void shrink_to_fit();

    iterator insert(const_iterator pos, T const &val);

    iterator erase(const_iterator pos);

    iterator erase(const_iterator first, const_iterator last);

    template<typename V>
    friend void swap(vector<V> &a, vector<V> &b);

    template<typename V>
    friend bool operator==(vector<V> const &a, vector<V> const &b) noexcept;

    template<typename V>
    friend bool operator!=(vector<V> const &a, vector<V> const &b) noexcept;

    template<typename V>
    friend bool operator<(vector<V> const &a, vector<V> const &b) noexcept;

    template<typename V>
    friend bool operator>(vector<V> const &a, vector<V> const &b) noexcept;

    template<typename V>
    friend bool operator<=(vector<V> const &a, vector<V> const &b) noexcept;

    template<typename V>
    friend bool operator>=(vector<V> const &a, vector<V> const &b) noexcept;

private:
    static size_t const SIZE_I = 0;
    static size_t const CAP_I = 1;
    static size_t const REF_I = 2;
    bool single;
    union {
        T val;
        size_t *buf;
    };

    size_t get_buf_size(size_t n) {
        return sizeof(size_t) * 3 + sizeof(T) * n;
    }

    T const *buf_start() const {
        return buf_start(buf);
    }

    T *buf_start() {
        return buf_start(buf);
    }

    T const *buf_start(size_t *buf) const {
        return reinterpret_cast<T *>(buf + 3);
    }

    T *buf_start(size_t *buf) {
        return reinterpret_cast<T *>(buf + 3);
    }

    bool has_buf() {
        return !single && !empty();
    }

    void make_unique() {
        if (!has_buf() || buf[REF_I] == 1) {
            return;
        }
        size_t *temp = new_buf_with_size(capacity());
        --buf[REF_I];
        buf = temp;
    }

    size_t *new_buf_with_size(size_t buf_size) {
        auto *new_buf = static_cast<size_t *>(operator new(get_buf_size(buf_size)));
        size_t new_size = std::min(buf_size, size());
        new_buf[SIZE_I] = new_size;
        new_buf[CAP_I] = buf_size;
        new_buf[REF_I] = 1;
        size_t i = 0;
        try {
            if (single) {
                new(buf_start(new_buf)) T(val);
            } else {
                for (; i < new_size; ++i) {
                    new(buf_start(new_buf) + i) T(*(buf_start() + i));
                }
            }
            return new_buf;
        } catch (...) {
            for (size_t j = 0; j < i; ++j) {
                (*(buf_start(new_buf) + j)).~T();
            }
            operator delete(new_buf);
            throw;
        }
    }
};

template<typename T>
vector<T>::vector() noexcept : single(false), buf(nullptr) {}

template<typename T>
vector<T>::vector(vector const &other) : single(other.single) {
    if (other.empty()) {
        buf = nullptr;
        return;
    }
    if (other.single) {
        new(&val) T(other.val);
        return;
    }
    buf = other.buf;
    ++buf[REF_I];
}

template<typename T>
vector<T> &vector<T>::operator=(vector const &other) {
    if (this == &other) {
        return *this;
    }
    if (other.empty()) {
        clear();
        single = false;
        buf = nullptr;
        return *this;
    }
    if (other.single) {
        vector copy(*this);
        clear();
        try {
            new(&val) T(other.val);
            single = true;
        } catch (...) {
            *this = copy;
            throw;
        }
        return *this;
    }
    clear();
    single = other.single;
    buf = other.buf;
    ++buf[REF_I];
    return *this;
}

template<typename T>
template<typename InputIterator>
vector<T>::vector(InputIterator first, InputIterator last) : single(false), buf(nullptr) {
    for (size_t i = 0; first != last; ++first, ++i) {
        try {
            push_back(*first);
        } catch (...) {
            for (size_t j = 0; j < i; ++j) {
                pop_back();
            }
            throw;
        }
    }
}

template<typename T>
vector<T>::~vector() {
    clear();
}

template<typename T>
template<typename InputIterator>
void vector<T>::assign(InputIterator first, InputIterator last) {
    vector copy(*this);
    copy.clear();
    for (; first != last; ++first) {
        copy.push_back(*first);
    }
    *this = copy;
}

template<typename T>
T &vector<T>::operator[](size_t index) {
    if (single) {
        assert(index == 0);
        return val;
    }
    make_unique();
    return *(buf_start() + index);
}

template<typename T>
T const &vector<T>::operator[](size_t index) const {
    if (single) {
        assert(index == 0);
        return val;
    }
    return *(buf_start() + index);
}

template<typename T>
T &vector<T>::front() noexcept {
    assert(!empty());
    return single ? val : (*this)[0];
}

template<typename T>
T const &vector<T>::front() const noexcept {
    assert(!empty());
    return single ? val : (*this)[0];
}

template<typename T>
T &vector<T>::back() noexcept {
    assert(!empty());
    return single ? val : (*this)[size() - 1];
}

template<typename T>
T const &vector<T>::back() const noexcept {
    assert(!empty());
    return single ? val : (*this)[size() - 1];
}

template<typename T>
void vector<T>::push_back(T const &value) {
    if (empty()) {
        try {
            new(&val) T(value);
            single = true;
        } catch (...) {
            buf = nullptr;
            single = false;
            throw;
        }
        return;
    }
    size_t *temp = nullptr;
    if (single || size() == capacity()) {
        size_t n = 2 * size();
        if (!(n <= 1 || (has_buf() && capacity() >= n))) {
            temp = new_buf_with_size(n);
        }
    }
    if (temp) {
        try {
            new(buf_start(temp) + size()) T(value);
            clear();
            buf = temp;
        } catch (...) {
            for (size_t j = 0; j < size(); ++j) {
                (*(buf_start(temp) + j)).~T();
            }
            operator delete(temp);
            throw;
        }
    } else {
        new(buf_start() + size()) T(value);
    }
    ++buf[SIZE_I];
}

template<typename T>
void vector<T>::pop_back() {
    assert(size() > 0);
    if (single) {
        clear();
        return;
    }
    make_unique();
    (*this)[size() - 1].~T();
    --buf[SIZE_I];
    if (size() == 0) {
        clear();
    }
}

template<typename T>
typename vector<T>::iterator vector<T>::begin() {
    if (empty()) {
        return nullptr;
    }
    if (single) {
        return &val;
    }
    return &(*this)[0];
}

template<typename T>
typename vector<T>::const_iterator vector<T>::begin() const noexcept {
    if (empty()) {
        return nullptr;
    }
    if (single) {
        return &val;
    }
    return &(*this)[0];
}

template<typename T>
typename vector<T>::iterator vector<T>::end() {
    if (empty()) {
        return nullptr;
    }
    if (single) {
        iterator it = &val;
        ++it;
        return it;
    }
    return buf_start() + size();
}

template<typename T>
typename vector<T>::const_iterator vector<T>::end() const noexcept {
    if (empty()) {
        return nullptr;
    }
    if (single) {
        const_iterator it = &val;
        ++it;
        return it;
    }
    return buf_start() + size();
}

template<typename T>
T *vector<T>::data() {
    return begin();
}

template<typename T>
T const *vector<T>::data() const noexcept {
    return begin();
}

template<typename T>
bool vector<T>::empty() const noexcept {
    return !single && !buf;
}

template<typename T>
size_t vector<T>::size() const noexcept {
    if (empty()) {
        return 0;
    }
    if (single) {
        return 1;
    }
    return buf[SIZE_I];
}

template<typename T>
size_t vector<T>::capacity() const noexcept {
    if (empty()) {
        return 0;
    }
    if (single) {
        return 1;
    }
    return buf[CAP_I];
}

template<typename T>
void vector<T>::clear() noexcept {
    if (single) {
        val.~T();
    } else if (buf) {
        if (buf[REF_I] == 1) {
            for (size_t i = 0; i < size(); ++i) {
                (*this)[i].~T();
            }
            operator delete(buf);
        } else {
            --buf[REF_I];
        }
    }
    single = false;
    buf = nullptr;
}

template<typename T>
void vector<T>::reserve(size_t n) {
    if (n <= 1 || (has_buf() && capacity() >= n)) {
        return;
    }
    size_t *temp = new_buf_with_size(n);
    clear();
    buf = temp;
}

template<typename T>
void vector<T>::resize(size_t n, T const &val) {
    if (n == size()) {
        return;
    }
    size_t *temp = new_buf_with_size(n);
    clear();
    buf = temp;
    for (size_t i = size(); i < n; ++i) {
        try {
            push_back(val);
        } catch (...) {
            for (size_t j = size(); j < i; ++j) {
                pop_back(); // vector is unique -> strong
            }
            throw;
        }
    }
}

template<typename T>
void vector<T>::shrink_to_fit() {
    if (!has_buf() || size() == capacity()) {
        return;
    }
    size_t *temp = new_buf_with_size(size());
    clear();
    buf = temp;
}

template<typename T>
typename vector<T>::iterator vector<T>::insert(vector::const_iterator pos, T const &value) {
    size_t index = pos - begin();
    if (index == size()) {
        push_back(value);
        return begin() + index;
    }
    T val_copy(value);
    if (size() < capacity()) {
        make_unique();
    } else {
        reserve(2 * size());
    }
    size_t i = size();
    ++buf[SIZE_I];
    try {
        for (; i > index; --i) {
            new(buf_start() + i) T(*(buf_start() + i - 1));
            (*(buf_start() + i - 1)).~T();
        }
        new(buf_start() + index) T(val_copy);
        return begin() + index;
    } catch (...) {
        for (size_t j = 0; j < size(); ++j) {
            if (j != i) {
                (*(buf_start() + j)).~T();
            }
        }
        operator delete(buf);
        buf = nullptr;
        throw;
    }
}

template<typename T>
typename vector<T>::iterator vector<T>::erase(vector::const_iterator pos) {
    return erase(pos, pos + 1);
}

template<typename T>
typename vector<T>::iterator vector<T>::erase(vector::const_iterator first, vector::const_iterator last) {
    size_t left = first - begin();
    size_t right = last - begin();
    size_t new_size = size() + left - right;
    if (new_size == 0) {
        clear();
        return nullptr;
    }
    size_t old_size = size();
    buf[SIZE_I] = new_size;
    size_t i = left;
    try {
        for (; i < old_size; ++i) {
            (*(buf_start() + i)).~T();
            if (i < old_size - (right - left)) {
                new(buf_start() + i) T(*(buf_start() + i + (right - left)));
            }
        }
        return begin() + left;
    } catch (...) {
        for (size_t j = 0; j < old_size; ++j) {
            if (j != i) {
                (*(buf_start() + j)).~T();
            }
        }
        operator delete(buf);
        buf = nullptr;
        throw;
    }
}

template<typename T>
typename vector<T>::reverse_iterator vector<T>::rbegin() {
    return reverse_iterator(end());
}

template<typename T>
typename vector<T>::const_reverse_iterator vector<T>::rbegin() const noexcept {
    return const_reverse_iterator(end());
}

template<typename T>
typename vector<T>::reverse_iterator vector<T>::rend() {
    return reverse_iterator(begin());
}

template<typename T>
typename vector<T>::const_reverse_iterator vector<T>::rend() const noexcept {
    return const_reverse_iterator(begin());
}


template<typename T>
void swap(vector<T> &a, vector<T> &b) {
    using std::swap;
    if (a.single && b.single) {
        swap(a.val, b.val);
        return;
    }
    if (!a.single && !b.single) {
        swap(a.buf, b.buf);
        return;
    }
    if (a.single && !b.single) {
        T a_val(a.val);
        a.single = false;
        a.val.~T();
        a.buf = b.buf;
        b.buf = nullptr;
        new(&b.val) T(a_val);
        b.single = true;
    } else {
        swap(b, a);
    }
}

template<typename T>
bool operator==(vector<T> const &a, vector<T> const &b) noexcept {
    if (a.size() != b.size()) {
        return false;
    }
    for (size_t i = 0; i < a.size(); ++i) {
        if (a[i] != b[i]) {
            return false;
        }
    }
    return true;
}

template<typename T>
bool operator!=(vector<T> const &a, vector<T> const &b) noexcept {
    return !(a == b);
}

template<typename T>
bool operator<(vector<T> const &a, vector<T> const &b) noexcept {
    for (size_t i = 0; i < std::min(a.size(), b.size()); ++i) {
        if (a[i] != b[i]) {
            return a[i] < b[i];
        }
    }
    return a.size() < b.size();
}

template<typename T>
bool operator>(vector<T> const &a, vector<T> const &b) noexcept {
    return b < a;
}

template<typename T>
bool operator<=(vector<T> const &a, vector<T> const &b) noexcept {
    return !(a > b);
}

template<typename T>
bool operator>=(vector<T> const &a, vector<T> const &b) noexcept {
    return !(a < b);
}