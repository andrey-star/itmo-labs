#ifndef HUFFMANCODING_BUFFERED_READER_H
#define HUFFMANCODING_BUFFERED_READER_H

#include <istream>

#include <constants.h>

struct buffered_reader {
public:
    explicit buffered_reader(std::istream &stream);

    bool read_char(unsigned char &x);

    void reset();

private:
    void fill_buffer();

    char buffer[BUFFER_SIZE];
    std::streamsize pos, size;
    std::istream &stream;

};

#endif //HUFFMANCODING_BUFFERED_READER_H
