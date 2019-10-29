#include "big_integer.h"

#include <cstring>
#include <iostream>
#include <algorithm>

const std::vector<int32_t> POW_10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};
const ui MAX_10 = std::numeric_limits<ui>::digits10;
const ui MAX_2 = std::numeric_limits<ui>::digits;
const ui MAX_UINT = std::numeric_limits<ui>::max();
const ull BASE = static_cast<ull>(MAX_UINT) + 1;

void big_integer::trim_leading_digits() {
    digits.assure_modifiable();
    while (digits.size() > 1 && digits.back() == sign) {
        digits.pop_back();
    }

    if (digits.size() == 1 && digits.back() == 0) {
        sign = 0;
    }
}

big_integer::big_integer() : sign(0), digits({0}) {}

big_integer::big_integer(big_integer const &other) = default;

big_integer::big_integer(int a) : sign(a < 0 ? MAX_UINT : 0), digits(static_cast<ui>(a)) {}

big_integer::big_integer(ui a) : sign(0), digits(a) {}

big_integer::big_integer(std::string const &str) : big_integer() {
    ui res_sign = -(str[0] == '-');
    for (size_t i = (res_sign) ? 1 : 0; i < str.size(); i += MAX_10) {
        if (i + MAX_10 < str.size()) {
            *this *= POW_10.back();
            if (res_sign) {
                *this -= std::stoi(str.substr(i, MAX_10));
            } else {
                *this += std::stoi(str.substr(i, MAX_10));
            }
        } else {
            *this *= POW_10[str.size() - i];
            if (res_sign) {
                *this -= std::stoi(str.substr(i, str.size() - i));
            } else {
                *this += std::stoi(str.substr(i, str.size() - i));
            }
        }
    }
    sign = res_sign;
    trim_leading_digits();
}

big_integer::~big_integer() = default;

ui big_integer::get_digit(size_t index) const {
    return (index < digits.size() ? digits[index] : sign);
}

big_integer &big_integer::operator=(big_integer const &other) = default;

template<bool mode>
big_integer &big_integer::addsub(big_integer const &rhs) {
    digits.assure_modifiable();
    bool carry = false;
    digits.resize(std::max(digits.size(), rhs.digits.size()) + 2, sign);
    for (size_t i = 0; i < digits.size(); ++i) {
        long long diff = 1ull * rhs.get_digit(i) + carry;
        long long res = get_digit(i) + (mode ? diff : -diff);
        digits[i] = static_cast <ui>(res);
        carry = (mode ? res > MAX_UINT : res < 0);
    }
    sign = digits.back();
    trim_leading_digits();
    return *this;
}

big_integer &big_integer::operator+=(big_integer const &rhs) {
    return addsub<true>(rhs);
}

big_integer &big_integer::operator-=(big_integer const &rhs) {
    return addsub<false>(rhs);
}

big_integer &big_integer::operator*=(big_integer const &rhs) {
    return *this = (*this * rhs);
}

std::pair<big_integer, ui> divmod_by_digit(big_integer a, ui digit) {
    a.digits.assure_modifiable();
    ui carry = 0;
    for (size_t i = a.digits.size(); i-- > 0;) {
        ull cur = a.digits[i] + carry * BASE;
        a.digits[i] = static_cast<ui>(cur / digit);
        carry = static_cast<ui>(cur % digit);
    }
    a.trim_leading_digits();
    return {a, carry};
}

std::pair<big_integer, big_integer> divmod(big_integer const &a, big_integer const &b) {
    ui q_sign = a.sign ^b.sign;
    ui r_sign = a.sign;
    big_integer a_abs = a.abs();
    big_integer b_abs = b.abs();
    ui norm = static_cast<ui>(BASE / (1ull * b_abs.digits.back() + 1));
    a_abs *= norm;
    b_abs *= norm;
    big_integer q, r;
    q.digits.resize(a_abs.digits.size(), 0);
    for (size_t i = a_abs.digits.size(); i-- > 0;) {
        r.digits.push_front(0);
        r += a_abs.get_digit(i);
        ui s1 = r.get_digit(b_abs.digits.size());
        ui s2 = r.get_digit(b_abs.digits.size() - 1);
        ui d = static_cast<ui>((BASE * s1 + s2) / b_abs.digits.back());
        r -= b_abs * d;
        while (r.sign) {
            r += b_abs;
            --d;
        }
        q.digits[i] = d;
    }
    if (q_sign) {
        q = -q;
    }
    q.trim_leading_digits();
    r = divmod_by_digit(r, norm).first;
    if (r_sign) {
        r = -r;
    }
    r.trim_leading_digits();
    return {q, r};
}

big_integer &big_integer::operator/=(big_integer const &rhs) {
    return *this = divmod(*this, rhs).first;
}

big_integer &big_integer::operator%=(big_integer const &rhs) {
    return *this = divmod(*this, rhs).second;
}

template<typename T>
big_integer &big_integer::logical_operation(big_integer const &rhs, T const &op) {
    digits.assure_modifiable();
    digits.resize(std::max(digits.size(), rhs.digits.size()), sign);
    for (size_t i = 0; i < digits.size(); i++) {
        digits[i] = op(get_digit(i), rhs.get_digit(i));
    }
    sign = op(sign, rhs.sign);
    trim_leading_digits();
    return *this;
}

big_integer &big_integer::operator&=(big_integer const &rhs) {
    return logical_operation(rhs, std::bit_and<>());
}

big_integer &big_integer::operator|=(big_integer const &rhs) {
    return logical_operation(rhs, std::bit_or<>());
}

big_integer &big_integer::operator^=(big_integer const &rhs) {
    return logical_operation(rhs, std::bit_xor<>());
}

big_integer &big_integer::operator<<=(int rhs) {
    if (rhs < 0) {
        return *this >>= (-rhs);
    }
    if (rhs == 0) {
        return *this;
    }
    digits.assure_modifiable();
    digits.push_front(0, rhs / MAX_2);
    digits.push_back(sign);
    ui carry = 0;
    ui left = rhs % MAX_2;
    for (size_t i = 0; i < digits.size(); i++) {
        ui old_digit = digits[i];
        digits[i] = ((digits[i] << left) | carry);
        carry = (old_digit >> (MAX_2 - left));
    }
    trim_leading_digits();
    return *this;
}

big_integer &big_integer::operator>>=(int rhs) {
    if (rhs < 0) {
        return *this <<= (-rhs);
    }
    digits.assure_modifiable();
    digits.pop_front(std::min<size_t>(digits.size(), rhs / MAX_2));
    ui carry = sign;

    unsigned left = rhs % MAX_2;
    for (size_t i = digits.size(); i-- > 0;) {
        ui old_digit = digits[i];
        digits[i] = ((carry << (MAX_2 - left)) | (digits[i] >> left));
        carry = (old_digit & ((1u << left) - 1));
    }
    trim_leading_digits();
    return *this;
}

big_integer big_integer::operator+() const {
    return *this;
}

big_integer big_integer::operator-() const {
    return ~*this + 1;
}

big_integer big_integer::operator~() const {
    big_integer res = *this;
    res.digits.assure_modifiable();
    res.sign = ~res.sign;
    for (size_t i = 0; i < res.digits.size(); i++) {
        res.digits[i] = ~res.digits[i];
    }
    return res;
}

big_integer &big_integer::operator++() {
    *this += 1;
    return *this;
}

const big_integer big_integer::operator++(int) {
    const big_integer res(*this);
    ++(*this);
    return res;
}

big_integer &big_integer::operator--() {
    *this -= 1;
    return *this;
}

const big_integer big_integer::operator--(int) {
    const big_integer res(*this);
    --(*this);
    return res;
}

big_integer operator+(big_integer a, big_integer const &b) {
    return a += b;
}

big_integer operator-(big_integer a, big_integer const &b) {
    return a -= b;
}

big_integer multiply_by_digit(big_integer a, ui digit) {
    a.digits.assure_modifiable();
    ui carry = 0;
    for (size_t i = 0; i < a.digits.size(); ++i) {
        ull cur = 1ull * a.get_digit(i) * digit + carry;
        carry = static_cast<ui>(cur / BASE);
        a.digits[i] = static_cast<ui>(cur % BASE);
    }
    if (carry) {
        a.digits.push_back(carry);
    }
    a.trim_leading_digits();
    return a;
}

big_integer operator*(big_integer const &a, big_integer const &b) {
    big_integer res;
    ui res_sign = a.sign ^b.sign;
    big_integer a_abs = a.abs();
    big_integer b_abs = b.abs();
    a_abs.digits.assure_modifiable();
    b_abs.digits.assure_modifiable();
    if (a_abs > b_abs) {
        a_abs.digits.swap(b_abs.digits);
    }
    for (size_t i = 0; i < a_abs.digits.size(); i++) {
        res += multiply_by_digit(b_abs, a_abs.digits[i]);
        b_abs.digits.push_front(0);
    }
    if (res_sign) {
        res = -res;
    }
    res.trim_leading_digits();
    return res;
}

big_integer operator/(big_integer a, big_integer const &b) {
    return a /= b;
}

big_integer operator%(big_integer a, big_integer const &b) {
    return a %= b;
}

big_integer operator&(big_integer a, big_integer const &b) {
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const &b) {
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const &b) {
    return a ^= b;
}

big_integer operator<<(big_integer a, int b) {
    return a <<= b;
}

big_integer operator>>(big_integer a, int b) {
    return a >>= b;
}

bool operator==(big_integer const &a, big_integer const &b) {
    return !(a < b) && !(b < a);
}

bool operator!=(big_integer const &a, big_integer const &b) {
    return !(a == b);
}

bool operator<(big_integer const &a, big_integer const &b) {
    if (a.sign != b.sign) {
        return a.sign > b.sign;
    }
    if (a.digits.size() != b.digits.size()) {
        return a.sign ? a.digits.size() > b.digits.size() : a.digits.size() < b.digits.size();
    }
    for (size_t i = a.digits.size(); i-- > 0;) {
        if (a.digits[i] != b.digits[i]) {
            return a.digits[i] < b.digits[i];
        }
    }
    return false;
}

bool operator>(big_integer const &a, big_integer const &b) {
    return b < a;
}

bool operator<=(big_integer const &a, big_integer const &b) {
    return !(a > b);
}

bool operator>=(big_integer const &a, big_integer const &b) {
    return !(a < b);
}

big_integer big_integer::abs() const {
    big_integer res = *this;
    return sign ? -res : res;
}

std::string to_string(big_integer const &a) {
    std::string res;
    big_integer a_abs = a.abs();
    big_integer zero(0);
    std::pair<big_integer, ui> p;
    do {
        p = divmod_by_digit(a_abs, 10);
        res += std::to_string(p.second);
        a_abs = p.first;
    } while (a_abs != zero);
    if (a.sign) {
        res.append("-");
    }
    std::reverse(res.begin(), res.end());
    return res;
}

std::ostream &operator<<(std::ostream &s, big_integer const &a) {
    return s << to_string(a);
}
