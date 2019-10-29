#include "buffered_reader.h"

buffered_reader::buffered_reader(std::istream &stream) : stream(stream) {
    fill_buffer();
}

bool buffered_reader::read_char(unsigned char &x) {
    if (pos < size) {
        x = buffer[pos++];
        return true;
    } else {
        fill_buffer();
        if (pos < size) {
            x = buffer[pos++];
            return true;
        } else {
            return false;
        }
    }
}

void buffered_reader::reset() {
    stream.clear();
    stream.seekg(0);
}

void buffered_reader::fill_buffer() {
    stream.read(buffer, BUFFER_SIZE);
    size = stream.gcount();
    pos = 0;
}