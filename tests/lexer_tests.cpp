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

void test_token_accepts(
    std::function<std::optional<Token>(std::string &, size_t &)> f,
    TokenType type, std::string &test, std::string &expected, size_t pos,
    size_t expected_pos,
    std::optional<std::vector<size_t>> &expected_variables_positions) {
    std::optional<Token> parsed = f(test, pos);
    ASSERT_TRUE(parsed.has_value());
    ASSERT_EQ(expected, parsed.value().content);
    ASSERT_EQ(type, parsed.value().type);
    ASSERT_EQ(expected_pos, pos);
    ASSERT_EQ(expected_variables_positions.has_value(),
              parsed.value().variables_positions.has_value());
    if (expected_variables_positions.has_value()) {
        ASSERT_EQ(expected_variables_positions.value().size(),
                  parsed.value().variables_positions.value().size());
        for (size_t i = 0; i < expected_variables_positions.value().size();
             i++) {
            ASSERT_EQ(expected_variables_positions.value()[i],
                      parsed.value().variables_positions.value()[i]);
        }
    }
}

void test_token_rejects(
    std::function<std::optional<Token>(std::string &, size_t &)> f,
    std::string &test, size_t pos, size_t expected_pos) {
    std::optional<Token> parsed = f(test, pos);
    ASSERT_FALSE(parsed.has_value());
    ASSERT_EQ(expected_pos, pos);
}

using token_test_case_t = std::tuple<std::string, std::string, size_t, size_t,
                                     std::optional<std::vector<size_t>>>;

///////////////////////////////////////////////////////////////////////////////
// Lexer::parse_single_qs TESTS
///////////////////////////////////////////////////////////////////////////////

TEST(LexerTests, TestParseSingleQSAcceptsInstantClose) {
    token_test_case_t test_case = {"\'", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSAcceptsSimpleString) {
    token_test_case_t test_case = {"Simple\'", "Simple", 0, 7, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSAcceptsInstantCloseWithSpacesAfter) {
    token_test_case_t test_case = {"\'  ", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSAcceptsSimpleStringWithSpacesAfter) {
    token_test_case_t test_case = {"SimpleSpaced\'  ", "SimpleSpaced", 0, 13,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSAcceptsSingleQInserted) {
    token_test_case_t test_case = {"SingleQIns\\\'ide\'", "SingleQIns\\", 0, 12,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSAcceptsDollars) {
    token_test_case_t test_case = {"$$Dollars\'", "$$Dollars", 0, 10,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSAcceptsSpacesAndBackslashes) {
    token_test_case_t test_case = {"With spaces\\ and backslash\'",
                                   "With spaces\\ and backslash", 0, 27,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_single_qs_p, SingleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseSingleQSRejectsEmptyNotClosed) {
    token_test_case_t test_case = {"", "", 0, 0, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_single_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseSingleQSRejectsSpacesNotClosed) {
    token_test_case_t test_case = {"   ", "", 0, 3, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_single_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseSingleQSRejectsTextNotClosed) {
    token_test_case_t test_case = {"Not closed", "", 0, 10, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_single_qs_p, test, pos, exp_pos);
}

///////////////////////////////////////////////////////////////////////////////
// Lexer::parse_word TESTS
///////////////////////////////////////////////////////////////////////////////

TEST(LexerTests, TestParseWordAcceptsSimpleWord) {
    token_test_case_t test_case = {"Simple", "Simple", 0, 6, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_word_p, Word, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseWordAcceptsDottedWord) {
    token_test_case_t test_case = {".dotted", ".dotted", 0, 7, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_word_p, Word, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseWordAcceptsSpacedWord) {
    token_test_case_t test_case = {"Spaced ", "Spaced", 0, 6, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_word_p, Word, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseWordAcceptsSpecialSimbols) {
    token_test_case_t test_case = {".special_simbols_", ".special_simbols_", 0,
                                   17, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_word_p, Word, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseWordAcceptsSimplePath) {
    token_test_case_t test_case = {"../path/with/slash", "../path/with/slash",
                                   0, 18, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_word_p, Path, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseWordAcceptsPathSpaced) {
    token_test_case_t test_case = {"../path/with/slash_spaced_end   ",
                                   "../path/with/slash_spaced_end", 0, 29,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_word_p, Path, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseWordRejectsEmpty) {
    token_test_case_t test_case = {"", "", 0, 0, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_word_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseWordRejectsOnlySpaces) {
    token_test_case_t test_case = {"   ", "", 0, 0, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_word_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseWordRejectsWrongSpecial) {
    token_test_case_t test_case = {"!", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_word_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseWordRejectsDollars) {
    token_test_case_t test_case = {"$", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_word_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseWordRejectsSingleQ) {
    token_test_case_t test_case = {"\'", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_word_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseWordRejectsDoubleQ) {
    token_test_case_t test_case = {"\"", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_word_p, test, pos, exp_pos);
}

///////////////////////////////////////////////////////////////////////////////
// Lexer::parse_variable TESTS
///////////////////////////////////////////////////////////////////////////////

TEST(LexerTests, TestParseVariableAcceptsSimple) {
    token_test_case_t test_case = {"Simple", "Simple", 0, 6, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_variable_p, Variable, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseVariableAcceptsSimpleSpaced) {
    token_test_case_t test_case = {"Simple   ", "Simple", 0, 6, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_variable_p, Variable, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseVariableAcceptsUnderscores) {
    token_test_case_t test_case = {"Sim_ple", "Sim_ple", 0, 7, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_variable_p, Variable, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseVariableRejectsEmpty) {
    token_test_case_t test_case = {"", "", 0, 0, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_variable_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseVariableRejectsOnlySpaces) {
    token_test_case_t test_case = {"   ", "", 0, 0, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_variable_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseVariableRejectsOtherSymbols) {
    token_test_case_t test_case = {"S!mple", "", 0, 2, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_variable_p, test, pos, exp_pos);
}

///////////////////////////////////////////////////////////////////////////////
// Lexer::parse_double_qs TESTS
///////////////////////////////////////////////////////////////////////////////

TEST(LexerTests, TestParseDoubleQSAcceptsInstantClose) {
    token_test_case_t test_case = {"\"", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSpacedAfterClose) {
    token_test_case_t test_case = {"\"   ", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSimpleString) {
    token_test_case_t test_case = {"Simple\"", "Simple", 0, 7, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSimpleStringWithSpaces) {
    token_test_case_t test_case = {"  Simple str!ng with sp@ces.\"",
                                   "  Simple str!ng with sp@ces.", 0, 29,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSimpleStringWithVariable) {
    token_test_case_t test_case = {
        "Simple str!ng with $Variable\"", "Simple str!ng with $Variable", 0, 29,
        std::make_optional(std::vector<size_t>({19}))};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSimpleStringWithMultipleVariables) {
    token_test_case_t test_case = {
        "Simple $string with $Variable\"", "Simple $string with $Variable", 0,
        30, std::make_optional(std::vector<size_t>({7, 20}))};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSimpleStringWithBackslashedDollar) {
    token_test_case_t test_case = {"Dollar \\$\"", "Dollar $", 0, 10,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSAcceptsSimpleStringWithMultipleBackslashes) {
    token_test_case_t test_case = {"\\$\\\'\\\"\"", "$\'\"", 0, 7,
                                   std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests,
     TestParseDoubleQSAcceptsSimpleStringWithBackslashesAndVariables) {
    token_test_case_t test_case = {
        "$Var1 \\$Var2 $Var3 \\$Var4 $Var5\"", "$Var1 $Var2 $Var3 $Var4 $Var5",
        0, 32, std::make_optional(std::vector<size_t>({0, 12, 24}))};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests,
     TestParseDoubleQSAcceptsSimpleStringWithBackslashesAndVariablesShifted) {
    token_test_case_t test_case = {
        "   $Var1 \\$Var2 $Var3 \\$Var4 $Var5\"",
        "$Var1 $Var2 $Var3 $Var4 $Var5", 3, 35,
        std::make_optional(std::vector<size_t>({0, 12, 24}))};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_double_qs_p, DoubleQS, test, expected,
                       pos, exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseDoubleQSRejectsEmptyNotClosed) {
    token_test_case_t test_case = {"", "", 0, 0, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_double_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseDoubleQSRejectsSpacesNotClosed) {
    token_test_case_t test_case = {"   ", "", 0, 3, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_double_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseDoubleQSRejectsTextNotClosed) {
    token_test_case_t test_case = {"Not closed", "", 0, 10, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_double_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseDoubleQSRejectsWithVariableNotClosed) {
    token_test_case_t test_case = {"$var", "", 0, 4, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_double_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseDoubleQSRejectsWithVariableBadSymbol) {
    token_test_case_t test_case = {"$var!\"", "", 0, 5, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_double_qs_p, test, pos, exp_pos);
}

TEST(LexerTests, TestParseDoubleQSRejectsWithEmptyVariable) {
    token_test_case_t test_case = {"$\"", "", 0, 1, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_rejects(TestLexer::parse_double_qs_p, test, pos, exp_pos);
}

///////////////////////////////////////////////////////////////////////////////
// Lexer::parse_token TESTS
///////////////////////////////////////////////////////////////////////////////

TEST(LexerTests, TestParseTokenAcceptsWord) {
    token_test_case_t test_case = {"cat", "cat", 0, 3, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, Word, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsWordShifted) {
    token_test_case_t test_case = {"  cat", "cat", 0, 5, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, Word, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsVariable) {
    token_test_case_t test_case = {"$cat_", "cat_", 0, 5, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, Variable, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsVariableShifted) {
    token_test_case_t test_case = {"  $cat_", "cat_", 0, 7, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, Variable, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsSingleQS) {
    token_test_case_t test_case = {"\'Kitti-cat-ti cat\'", "Kitti-cat-ti cat",
                                   0, 18, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, SingleQS, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsSingleQSShifted) {
    token_test_case_t test_case = {"   \'Kitti-cat-ti cat\'",
                                   "Kitti-cat-ti cat", 0, 21, std::nullopt};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, SingleQS, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsDoubleQS) {
    token_test_case_t test_case = {
        "\"Kitti-cat-ti $cat\"", "Kitti-cat-ti $cat", 0, 19,
        std::make_optional(std::vector<size_t>({13}))};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, DoubleQS, test, expected, pos,
                       exp_pos, exp_var_pos);
}

TEST(LexerTests, TestParseTokenAcceptsDoubleQSShifted) {
    token_test_case_t test_case = {
        "   \"Kitti-cat-ti $cat\"", "Kitti-cat-ti $cat", 0, 22,
        std::make_optional(std::vector<size_t>({13}))};
    auto &[test, expected, pos, exp_pos, exp_var_pos] = test_case;
    test_token_accepts(TestLexer::parse_token_p, DoubleQS, test, expected, pos,
                       exp_pos, exp_var_pos);
}

///////////////////////////////////////////////////////////////////////////////
// Lexer::lex TESTS
///////////////////////////////////////////////////////////////////////////////

using lex_test_case_t = std::tuple<std::string, std::vector<Token>>;

void test_lex(lex_test_case_t &test_case) {
    auto &[test, expected_result] = test_case;
    auto result = Lexer::lex(test);
    ASSERT_EQ(expected_result.size(), result.size());
    for (size_t i = 0; i < result.size(); i++) {
        ASSERT_EQ(expected_result[i].type, result[i].type);
        ASSERT_EQ(expected_result[i].content, result[i].content);
        ASSERT_EQ(expected_result[i].variables_positions.has_value(),
                  result[i].variables_positions.has_value());
        if (result[i].variables_positions.has_value()) {
            auto res_vars = result[i].variables_positions.value();
            auto exp_vars = expected_result[i].variables_positions.value();
            for (size_t j = 0; j < res_vars.size(); j++) {
                ASSERT_EQ(exp_vars[j], res_vars[j]);
            }
        }
    }
}

TEST(LexerTests, TestLexAcceptsSingleWord) {
    lex_test_case_t test_case = {"cat",
                                 std::vector<Token>({Token(Word, "cat")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsMultipleWords) {
    lex_test_case_t test_case = {
        "cat dog .gay ../here",
        std::vector<Token>({Token(Word, "cat"), Token(Word, "dog"),
                            Token(Word, ".gay"), Token(Path, "../here")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsSingleVariable) {
    lex_test_case_t test_case = {"$cat",
                                 std::vector<Token>({Token(Variable, "cat")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsMultipleVariables) {
    lex_test_case_t test_case = {
        "$cat $dog_ ",
        std::vector<Token>({Token(Variable, "cat"), Token(Variable, "dog_")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsSingleSingleQS) {
    lex_test_case_t test_case = {"\'$cat\'",
                                 std::vector<Token>({Token(SingleQS, "$cat")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsMultipleSingleQS) {
    lex_test_case_t test_case = {
        "\'$cat\' \'Hello, world!\' \'How are you?\'",
        std::vector<Token>({Token(SingleQS, "$cat"),
                            Token(SingleQS, "Hello, world!"),
                            Token(SingleQS, "How are you?")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsSingleDoubleQS) {
    lex_test_case_t test_case = {"\"cat\"",
                                 std::vector<Token>({Token(DoubleQS, "cat")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsSingleDoubleQSWithVarsAndSlashes) {
    lex_test_case_t test_case = {
        "\"$Var1 \\$Var2 $Var3 \\$Var4 $Var5\"",
        std::vector<Token>({Token(DoubleQS, "$Var1 $Var2 $Var3 $Var4 $Var5",
                                  std::vector<size_t>({0, 12, 24}))})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsMultipleDoubleQSWithVarsAndSlashes) {
    lex_test_case_t test_case = {
        "\"$Var1 \\$Var2 $Var3 \\$Var4 $Var5\" "
        "\"$Var1 $Var2 $Var3 $Var4 $Var5\" "
        "\"\\$Var1 \\$Var2 \\$Var3 \\$Var4 \\$Var5\"",
        std::vector<Token>({Token(DoubleQS, "$Var1 $Var2 $Var3 $Var4 $Var5",
                                  std::vector<size_t>({0, 12, 24})),
                            Token(DoubleQS, "$Var1 $Var2 $Var3 $Var4 $Var5",
                                  std::vector<size_t>({0, 6, 12, 18, 24})),
                            Token(DoubleQS, "$Var1 $Var2 $Var3 $Var4 $Var5")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexAcceptsLongPipedCommand) {
    lex_test_case_t test_case = {
        "cat ./hello_world.tex | grep \"[0-9]+ [a-A] $1 \\$\" | echo ",
        std::vector<Token>(
            {Token(Word, "cat"), Token(Path, "./hello_world.tex"),
             Token(Pipe, "|"), Token(Word, "grep"),
             Token(DoubleQS, "[0-9]+ [a-A] $1 $", std::vector<size_t>({13})),
             Token(Pipe, "|"), Token(Word, "echo")})};
    test_lex(test_case);
}

TEST(LexerTests, TestLexCutsAfterFirstFailure) {
    lex_test_case_t test_case = {
        "cat ./hello_world.tex | grep \"[0-9]+ [a-A] $ \\$\" | echo ",
        std::vector<Token>({Token(Word, "cat"),
                            Token(Path, "./hello_world.tex"), Token(Pipe, "|"),
                            Token(Word, "grep")})};
    test_lex(test_case);
}

} // namespace
