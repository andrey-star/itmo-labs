#ifndef MY_VECTOR
#define MY_VECTOR

#include <cstddef>
#include <vector>
#include <memory>
#include <utility>

template <typename T>
struct my_vector {
    explicit my_vector(T const& x);

    T& operator[](size_t i);

    T const& operator[](size_t i) const;

    T& back();

    T const& back() const;

    bool empty() const;

    void pop_back();

    void resize(size_t n, T const& val);

    bool operator==(my_vector<T> const& rhs) const;

    size_t size() const;

    void swap(my_vector<T>& rhs);

    void push_back(T const& val);

    void push_front(T const& val, size_t n = 1);

    void pop_front(size_t n);

    void assure_modifiable();

private:
    size_t _size;
    bool is_small;

    T small;
    std::shared_ptr<std::vector<T>> vec;

    void make_big();
};

#endif  // MY_VECTOR
