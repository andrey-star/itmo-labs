#ifndef HUFFMANCODING_CONSTANTS_H
#define HUFFMANCODING_CONSTANTS_H

#include <stdint.h>

typedef uint32_t ui;
typedef unsigned char uc;

const std::size_t BUFFER_SIZE = 32768;
const std::size_t ALPHABET_SIZE = 256;
const ui BITS_IN_CHAR = 8;

#endif //HUFFMANCODING_CONSTANTS_H
