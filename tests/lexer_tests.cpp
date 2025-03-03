#include "lexer.hpp"
#include <tuple>

#include <gtest/gtest.h>

class TestLexer : Lexer {
  public:
    static std::optional<Token> parse_single_qs_p(std::string &data,
                                                  size_t &p) {
        return parse_single_qs(data, p);
    }
    static std::optional<Token> parse_variable_p(std::string &data, size_t &p) {
        return parse_variable(data, p);
    }
    static std::optional<Token> parse_double_qs_p(std::string &data,
                                                  size_t &p) {
        return parse_double_qs(data, p);
    }
    static std::optional<Token> parse_token_p(std::string &data, size_t &p) {
        return parse_token(data, p);
    }
    static std::optional<Token> parse_word_p(std::string &data, size_t &p) {
        return parse_word(data, p);
    }
};
namespace {

TEST(LexerTests, TestParseSingleQSAccepts) {
    std::vector<std::tuple<std::string, std::string, size_t, size_t>> tests = {
        {"\'", "", 0, 1},
        {"Simple\'", "Simple", 0, 7},
        {"\'  ", "", 0, 1},
        {"SimpleSpaced\'  ", "SimpleSpaced", 0, 13},
        {"SingleQIns\\\'ide\'", "SingleQIns\\", 0, 12},
        {"$$Dollars\'", "$$Dollars", 0, 10},
        {"With spaces\\ and backslash\'", "With spaces\\ and backslash", 0,
         27}};

    for (auto &[test, expected, pos, exp_pos] : tests) {
        std::optional<Token> parsed = TestLexer::parse_single_qs_p(test, pos);
        ASSERT_TRUE(parsed.has_value());
        ASSERT_EQ(expected, parsed.value().content);
        ASSERT_EQ(SingleQS, parsed.value().type);
        ASSERT_FALSE(parsed.value().variables_positions.has_value());
        ASSERT_EQ(exp_pos, pos);
    }
}

TEST(LexerTests, TestParseSingleQSRejects) {
    std::vector<std::tuple<std::string, std::string, size_t, size_t>> tests = {
        {"", "", 0, 0},
        {"   ", "", 0, 3},
        {"Not closed", "", 0, 10},
    };

    for (auto &[test, expected, pos, exp_pos] : tests) {
        std::optional<Token> parsed = TestLexer::parse_single_qs_p(test, pos);
        ASSERT_FALSE(parsed.has_value());
        ASSERT_EQ(exp_pos, pos);
    }
}

TEST(LexerTests, TestParseWordAccepts) {
    std::vector<std::tuple<std::string, std::string, size_t, size_t>> tests = {
        {"Simple", "Simple", 0, 6},
        {".dotted", ".dotted", 0, 7},
        {"Spaced ", "Spaced", 0, 6},
        {" Empty", "", 0, 0},
        {".special_simbols_", ".special_simbols_", 0, 17},
    };

    for (auto &[test, expected, pos, exp_pos] : tests) {
        std::optional<Token> parsed = TestLexer::parse_word_p(test, pos);
        ASSERT_TRUE(parsed.has_value());
        ASSERT_EQ(expected, parsed.value().content);
        ASSERT_EQ(Word, parsed.value().type);
        ASSERT_FALSE(parsed.value().variables_positions.has_value());
        ASSERT_EQ(exp_pos, pos);
    }
}

TEST(LexerTests, TestParsePathAccepts) {
    std::vector<std::tuple<std::string, std::string, size_t, size_t>> tests = {
        {"../path/with/slash", "../path/with/slash", 0, 18},
        {"../path/with/slash_spaced_end   ", "../path/with/slash_spaced_end", 0,
         29},
    };

    for (auto &[test, expected, pos, exp_pos] : tests) {
        std::optional<Token> parsed = TestLexer::parse_word_p(test, pos);
        ASSERT_TRUE(parsed.has_value());
        ASSERT_EQ(expected, parsed.value().content);
        ASSERT_EQ(Path, parsed.value().type);
        ASSERT_FALSE(parsed.value().variables_positions.has_value());
        ASSERT_EQ(exp_pos, pos);
    }
}

TEST(LexerTests, TestParseWordRejects) {
    std::vector<std::tuple<std::string, std::string, size_t, size_t>> tests = {
        {"", "", 0, 0},
        {"!", "", 0, 1},
        {"$", "", 0, 1},
        {"\'", "", 0, 1},
        {"\"", "", 0, 1}};

    for (auto &[test, expected, pos, exp_pos] : tests) {
        std::optional<Token> parsed = TestLexer::parse_word_p(test, pos);
        ASSERT_FALSE(parsed.has_value());
        ASSERT_EQ(exp_pos, pos);
    }
}

} // namespace
