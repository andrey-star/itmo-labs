#include <gtest/gtest.h>
#include <functional>
#include <random>
#include <algorithm>
#include <fstream>

#include "huffman.h"


void encode_decode(std::istream &in, std::ostream &out) {
    std::ostringstream encoded;
    huffman::encode(in, encoded);
    std::istringstream from_encoded(encoded.str());
    huffman::decode(from_encoded, out);
}


std::string encode_decode(std::string const &s) {
    std::istringstream in(s);
    std::ostringstream out;
    encode_decode(in, out);
    return out.str();
}

std::string random_string(std::size_t size) {
    std::string s(size, 0);
    std::mt19937::result_type seed = time(nullptr);
    auto rng = std::bind(std::uniform_int_distribution<int>(0, 255), std::mt19937(seed));
    std::generate(s.begin(), s.end(), rng);
    return s;
}

TEST(correctness, text) {
    std::string s = "09q89yu434q3vtc=]'4.qc34vq"
                    "q34co8tiqh34omgoqx3h98o4x87890-[;"
                    "  "
                    " "
                    " 4  q4t 98pyq83[tuy3q[4t  j4iqh347t 3q4tj q3t3q"
                    ""
                    ""
                    " "
                    "q34t9q38y4t 0q3t q34tq3t q3t 34y359013u 4-829ghcm957mg0h5w95yg84g  w45g pw45ghpw h5bw5 vgiw4"
                    "w 5gu4w5p g4wh5go8 hw5 7gph98o5wghw458ghwm4c58jg8tysqvn3p5y ghw895cgow[954 gw4%"
                    "gw4 5gw45hg wo85ghwc5yg8cnqh94 pm5g a;oh4]a4';gbk45sg945hq"
                    " q5g=q 5-=g4q5-9 8q475g9 05igj qbj5gn q45g q4"
                    "5g3 uqgq8n 5479gn87o5iv qho4g5l";
    EXPECT_EQ(s, encode_decode(s));
}


TEST(correctness, empty) {
    std::string s;
    EXPECT_EQ(s, encode_decode(s));
}

TEST(correctness, one) {
    std::string s = "1";
    EXPECT_EQ(s, encode_decode(s));
}

TEST(correctness, two) {
    std::string s = "2g";
    EXPECT_EQ(s, encode_decode(s));
}

TEST(correctness, three) {
    std::string s = "h7-";
    EXPECT_EQ(s, encode_decode(s));
}

TEST(correctness, other_ascii) {
    std::string s{0, 0, 0, 5, 7, 2, 12};
    EXPECT_EQ(s, encode_decode(s));
}

TEST(correctness, short_string) {
    std::string s = "jhgjk";
    EXPECT_EQ(s, encode_decode(s));
    std::string t = "fmgfvbhuy";
    EXPECT_EQ(t, encode_decode(t));
    std::string h = "kuygbjgvb";
    EXPECT_EQ(h, encode_decode(h));
}

TEST(correctness, long_string) {
    std::string t = "123456'''/.,mbvcx]9765rdhj6tfg";
    EXPECT_EQ(t, encode_decode(t));

    std::string h = "aygbnki876tfvbji76tfvbnkl;--";
    EXPECT_EQ(h, encode_decode(h));

    std::string k = "\x01 :6\xAB\xDE\x9C 8;''/";
    EXPECT_EQ(k, encode_decode(k));

    std::string s = "876tfvgre21qaz09]'//']'/.,m";
    EXPECT_EQ(s, encode_decode(s));
}


TEST(correctness, very_long_string) {
    std::string s = random_string(100000);
    EXPECT_EQ(s, encode_decode(s));
}

//TEST(correctness, encode) {
//    std::ifstream source;
//    std::ofstream destination;
//    std::string s = R"(C:\Program Files\Adobe\Adobe Photoshop CC 2018\AdobePDFL.dll)";
//    source.open(s, std::ios::binary);
//    destination.open("output.txt", std::ios::binary);
//    encode_decode(source, destination);
//}


TEST(correctness, random_strings_test) {
    static constexpr std::size_t MAX_LENGTH = 100;
    static constexpr std::size_t REPETITIONS_COUNT = 10;
    for (std::size_t len = 1; len < MAX_LENGTH; ++len) {
        for (std::size_t _ = 0; _ < REPETITIONS_COUNT; ++_) {
            std::string s = random_string(len);
            EXPECT_EQ(s, encode_decode(s));
        }
    }
}

TEST(invalid, incorrect_decode) {
    std::stringstream in0("");
    std::stringstream in1("random ennn");
    std::stringstream in2("0 0 1 0 random ennn");
    std::stringstream in3(std::string{'\7'} + "random ennn");
    std::stringstream in4("0 0 1f 6 8");
    std::stringstream out;

    EXPECT_ANY_THROW(huffman::decode(in0, out));
    EXPECT_ANY_THROW(huffman::decode(in1, out));
    EXPECT_ANY_THROW(huffman::decode(in2, out));
    EXPECT_ANY_THROW(huffman::decode(in3, out));
    EXPECT_ANY_THROW(huffman::decode(in4, out));
}

TEST(invalid, random_incorrect_decode) {
    for (std::size_t i = 0; i < 50; ++i) {
        std::string s = random_string(100000);
        std::stringstream in(s);
        std::stringstream out;
        EXPECT_ANY_THROW(huffman::decode(in, out));
    }
}