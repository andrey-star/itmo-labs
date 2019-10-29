#include "buffered_writer.h"

buffered_writer::buffered_writer(std::ostream &stream) : pos(0), stream(stream) {}

void buffered_writer::write_char(unsigned char const &x) {
    if (pos < BUFFER_SIZE) {
        buffer[pos] = x;
        ++pos;
    } else {
        empty_buffer();
        buffer[pos] = x;
        ++pos;
    }
}

buffered_writer::~buffered_writer() {
    empty_buffer();
}

void buffered_writer::empty_buffer() {
    stream.write(buffer, pos);
    pos = 0;
}
