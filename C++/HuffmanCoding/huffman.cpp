#include "huffman.h"

#include <queue>

struct huffman::node {
    uc value;
    ui freq;
    node *left, *right;

    node(uc value, ui freq) {
        left = nullptr;
        right = nullptr;
        this->value = value;
        this->freq = freq;
    }

    bool is_leaf() {
        return !(this->left || this->right);
    }
};

void huffman::delete_node(huffman::node *node) {
    if (!node) {
        return;
    }
    delete_node(node->left);
    delete_node(node->right);
    delete node;
}

struct huffman::node_wrapper {
    node *root;

    explicit node_wrapper(huffman::node *root) : root(root) {}

    ~node_wrapper() {
        delete_node(root);
    }
};

struct huffman::code {
    ui size_;
    ui value_;

    code() : size_(0), value_(0) {}

    code(ui size_, ui value) : size_(size_), value_(value) {}

    void add_one() {
        add_zero();
        value_ += 1;
    }

    void add_zero() {
        value_ <<= 1u;
        size_++;
    }

    ui size() {
        return size_;
    }

    ui value() {
        return value_;
    }

    bool get(size_t j) {
        return static_cast<bool>((value_ >> j) & 1u);
    }

    void add(code &code) {
        value_ <<= code.size();
        size_ += code.size();
        value_ += code.value_;
    }

};

struct huffman::compare {
    bool operator()(node *l, node *r) {
        return (l->freq > r->freq);
    }
};

void huffman::get_codes(node *root, const code &cur_code, std::vector<code> &codes) {
    if (root->is_leaf()) {
        codes[root->value] = cur_code;
        return;
    }
    code left = cur_code;
    code right = cur_code;
    left.add_zero();
    right.add_one();
    get_codes(root->left, left, codes);
    get_codes(root->right, right, codes);
}

huffman::node_wrapper huffman::build_huffman_tree(ui *freq) {
    std::priority_queue<huffman::node *, std::vector<huffman::node *>, compare> build;
    for (uc c = 0;; c++) {
        build.push(new huffman::node(c, freq[c]));
        if (c == 255) {
            break;
        }
    }
    while (build.size() > 1) {
        node *left = build.top();
        build.pop();
        node *right = build.top();
        build.pop();
        node *parent = new huffman::node('\0', left->freq + right->freq);
        parent->left = left;
        parent->right = right;
        build.push(parent);
    }
    return node_wrapper(build.top());
}

void huffman::gen_codes(ui *freq, std::vector<code> &codes) {
    node_wrapper root_wrapper = build_huffman_tree(freq);
    get_codes(root_wrapper.root, code(), codes);
}

void get_freq(buffered_reader &in, ui *freq) {
    uc c;
    while (in.read_char(c)) {
        freq[c]++;
    }
}

huffman::code huffman::print_full_chars_from_code(code &c, buffered_writer &out) {
    ui value = c.value();
    ui size = c.size();
    for (ui i = 0; i < size; i++) {
        if (size < BITS_IN_CHAR * (i + 1)) {
            ui new_size = (size - BITS_IN_CHAR * i);
            ui new_val = value & ((1u << new_size) - 1);
            return {new_size, new_val};
        } else {
            ui byte = (value >> (size - BITS_IN_CHAR * (i + 1)));
            auto ch = static_cast<uc>(byte & 0xffu);
            out.write_char(ch);
        }
    }
    return {};
}

void print_number(ui n, buffered_writer &out) {
    for (ui i = 4; i-- > 0;) {
        out.write_char(static_cast<uc>((n >> (BITS_IN_CHAR * i)) & 0xffu));
    }
}

void huffman::encode(std::istream &input, std::ostream &output) {
    ui freq[ALPHABET_SIZE] = {};

    // calculate frequencies
    buffered_reader in(input);
    get_freq(in, freq);
    in.reset();

    // generate char codes
    std::vector<code> codes(ALPHABET_SIZE);
    gen_codes(freq, codes);

    // print frequencies
    buffered_writer out(output);
    for (ui i : freq) {
        print_number(i, out);
    }

    // print last byte's size
    ui last_bits = 0;
    uc c;
    for (size_t i = 0; i < ALPHABET_SIZE; i++) {
        last_bits += (1ull * codes[i].size() * freq[i]) & 0b111u;
        last_bits &= 0b111u;
    }
    out.write_char(static_cast<uc>(last_bits));

    // print encoded string
    code cur_code, rest;
    while (in.read_char(c)) {
        cur_code.add(codes[c]);
        rest = print_full_chars_from_code(cur_code, out);
        cur_code = rest;
    }
    if (cur_code.size() > 0) {
        out.write_char(static_cast<uc>(cur_code.value()));
    }
    in.reset();
}


ui read_number(buffered_reader &in) {
    ui res = 0;
    uc c;
    for (ui i = 0; i < 4; i++) {
        if (!in.read_char(c)) {
            throw std::invalid_argument("Encoded file corrupted");
        }
        res <<= BITS_IN_CHAR;
        res += c;
    }
    return res;
}

void huffman::process_code(node *&cur_node, node *&root, code &code, buffered_writer &out, ui *freq) {
    for (size_t i = code.size(); i-- > 0;) {
        if (!code.get(i)) {
            cur_node = cur_node->left;
        } else {
            cur_node = cur_node->right;
        }
        if (cur_node->is_leaf()) {
            freq[cur_node->value]++;
            out.write_char(cur_node->value);
            cur_node = root;
        }
    }
}

void huffman::decode(std::istream &input, std::ostream &output) {
    ui freq[ALPHABET_SIZE] = {};

    // read frequencies
    buffered_reader in(input);
    for (ui &i : freq) {
        i = read_number(in);
    }

    // read last byte's size
    uc c;
    if (!in.read_char(c)) {
        throw std::invalid_argument("Encoded file corrupted");
    }
    ui last_bits = c;
    if (last_bits > 7) {
        throw std::invalid_argument("Encoded file corrupted");
    }

    // restore huffman tree
    node_wrapper root_wrapper = build_huffman_tree(freq);
    node *root = root_wrapper.root;

    ui res_freq[ALPHABET_SIZE];
    for (ui &i : res_freq) {
        i = 0;
    }

    // decode the input
    buffered_writer out(output);
    node *cur_node = root;
    code cur_code, last_code;
    bool started = false;
    while (in.read_char(c)) {
        cur_code = code(BITS_IN_CHAR, c);
        if (started) {
            process_code(cur_node, root, last_code, out, res_freq);
        }
        last_code = cur_code;
        started = true;
    }
    if (last_bits != 0) {
        last_code = code(last_bits, c);
    }
    process_code(cur_node, root, last_code, out, res_freq);

    // check if frequencies match
    for (size_t i = 0; i < ALPHABET_SIZE; i++) {
        if (freq[i] != res_freq[i]) {
            throw std::invalid_argument("Encoded file corrupted");
        }
    }
}