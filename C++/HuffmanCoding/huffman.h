#ifndef HUFFMANCODING_HUFFMAN_H
#define HUFFMANCODING_HUFFMAN_H

#include <fstream>
#include <queue>

#include <buffered_io/buffered_reader.h>
#include <buffered_io/buffered_writer.h>

struct huffman {
public:
    static void encode(std::istream &input, std::ostream &output);
    static void decode(std::istream &input, std::ostream &output);

private:
    struct node;
    struct node_wrapper;
    struct compare;
    struct code;

    static void delete_node(node *node);
    static void get_codes(node *root, const code &cur_code, std::vector<code> &codes);
    static node_wrapper build_huffman_tree(ui *freq);
    static void gen_codes(ui *freq, std::vector<code> &codes);
    static code print_full_chars_from_code(code &c, buffered_writer &out);
    static void process_code(node *&cur_node, node *&root, code &code, buffered_writer &out, ui *freq);
};

#endif //HUFFMANCODING_HUFFMAN_H
