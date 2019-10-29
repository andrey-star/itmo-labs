#include <string>
#include <iostream>
#include <fstream>
#include <ctime>
#include <iomanip>
#include "huffman.h"

int main(int argc, char **argv) {
    if (argc != 4 || (std::string(argv[1]) != "-c" && std::string(argv[1]) != "-d")) {
        std::cout << "Usage: -c/-d <input-file> <output-file>" << std::endl;
        return -1;
    }

    std::string cmd = argv[1];
    std::string src_path = argv[2];
    std::string dest_path = argv[3];

    std::ifstream src;
    std::ofstream dest;
    src.open(src_path, std::ios::binary);
    dest.open(dest_path, std::ios::binary);

    std::clock_t start = clock();
    int exit_code = 0;
    if (!src) {
        std::cerr << "Missing input file: " << src_path << "\n";
        exit_code = -1;
    } else if (cmd == "-c") {
        huffman::encode(src, dest);
    } else if (cmd == "-d"){
        try {
            huffman::decode(src, dest);
        } catch (std::logic_error &e) {
            std::cerr << e.what() << ": " << src_path << "\n";
            exit_code = -1;
        }
    }
    src.close();
    dest.close();
    std::cerr << "Finished in: " << std::setprecision(3) << 1.0 * (clock() - start) / CLOCKS_PER_SEC << " s" << "\n";
    return exit_code;
}


