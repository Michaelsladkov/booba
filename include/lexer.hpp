#pragma once
#include "interfaces.hpp"

class TestLexer;

class Lexer {

  private:
    static std::optional<Token> parse_single_qs(std::string &data,
                                                size_t &pointer);
    static std::optional<Token> parse_variable(std::string &data,
                                               size_t &pointer,
                                               bool allow_double_q = false);
    static std::optional<Token> parse_double_qs(std::string &data,
                                                size_t &pointer);
    static std::optional<Token> parse_token(std::string &data, size_t &pointer);
    static std::optional<Token> parse_word(std::string &data, size_t &pointer);

  public:
    static std::vector<Token> lex(std::string &data);

    friend class TestLexer;
};