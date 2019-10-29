#ifndef HUFFMANCODING_BUFFERED_WRITER_H
#define HUFFMANCODING_BUFFERED_WRITER_H

#include <ostream>
#include <constants.h>

struct buffered_writer {
public:
    explicit buffered_writer(std::ostream &stream);

    ~buffered_writer();

    void write_char(unsigned char const &x);

private:
    void empty_buffer();

    char buffer[BUFFER_SIZE];
    std::size_t pos;
    std::ostream &stream;
};

#endif //HUFFMANCODING_BUFFERED_WRITER_H
