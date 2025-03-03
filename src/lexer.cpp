#include "lexer.hpp"
#include <iostream>

std::vector<Token> Lexer::lex(std::string &data) {
    size_t pointer = 0;
    std::vector<Token> result;
    while (pointer < data.length()) {
        auto parsed_token = parse_token(data, pointer);
        if (!parsed_token.has_value()) {
            return result;
        }
        result.push_back(parsed_token.value());
    }
    return result;
}

std::optional<Token> Lexer::parse_single_qs(std::string &data,
                                            size_t &pointer) {
    std::string result = "";
    if (pointer >= data.length()) {
        return std::nullopt;
    }
    bool closed = false;
    while (pointer < data.length()) {
        if (data[pointer] == '\'') {
            closed = true;
            pointer++;
            break;
        }
        result += data[pointer++];
    }
    if (!closed) {
        return std::nullopt;
    }
    return std::make_optional(Token(SingleQS, std::move(result)));
}

std::optional<Token> Lexer::parse_variable(std::string &data, size_t &pointer,
                                           bool allow_double_q) {
    std::string result = "";
    if (pointer >= data.length()) {
        return std::nullopt;
    }
    while (
        pointer < data.length() &&
        ((allow_double_q && data[pointer] != '"' && !isspace(data[pointer])) ||
         (!allow_double_q && !isspace(data[pointer])))) {
        if (!isalnum(data[pointer]) && (data[pointer] != '_') &&
            (data[pointer] != '$')) {
            pointer++;
            return std::nullopt;
        }
        result += data[pointer++];
    }
    if (result == "") {
        return std::nullopt;
    }
    return std::make_optional(Token(Variable, std::move(result)));
}

std::optional<Token> Lexer::parse_double_qs(std::string &data,
                                            size_t &pointer) {
    std::string result = "";
    size_t starting_position = pointer;
    std::vector<size_t> variables_positions;
    if (pointer >= data.length()) {
        return std::nullopt;
    }
    bool backslash = false;
    bool closed = false;
    while (pointer < data.length()) {
        if (!backslash && data[pointer] == '"') {
            closed = true;
            pointer++;
            break;
        }
        if (!backslash && data[pointer] == '\\') {
            backslash = true;
            pointer++;
            starting_position += 1;
            continue;
        }
        if (!backslash && data[pointer] == '$') {
            variables_positions.push_back(pointer++ - starting_position);
            auto parsed_variable = parse_variable(data, pointer, true);
            if (!parsed_variable.has_value()) {
                return std::nullopt;
            }
            result += "$" + parsed_variable.value().content;
            continue;
        }
        result += data[pointer++];
        backslash = false;
    }
    if (!closed) {
        return std::nullopt;
    }
    return std::make_optional(variables_positions.size() == 0
                                  ? Token(DoubleQS, std::move(result))
                                  : Token(DoubleQS, std::move(result),
                                          std::move(variables_positions)));
}

std::optional<Token> Lexer::parse_word(std::string &data, size_t &pointer) {
    std::string result = "";
    if (pointer >= data.length()) {
        return std::nullopt;
    }
    TokenType type = Word;

    while (pointer < data.length() && !isspace(data[pointer])) {
        if (!isalnum(data[pointer]) && data[pointer] != '/' &&
            data[pointer] != '_' && data[pointer] != '.') {
            pointer++;
            return std::nullopt;
        }
        if (data[pointer] == '/') {
            type = Path;
        }
        result += data[pointer++];
    }

    if (result == "") {
        return std::nullopt;
    }

    return std::make_optional(Token(type, result));
}

std::optional<Token> Lexer::parse_token(std::string &data, size_t &pointer) {
    while (pointer < data.length() && isspace(data[pointer])) {
        pointer++;
    }
    if (pointer >= data.length()) {
        return std::nullopt;
    }

    switch (data[pointer++]) {
    case '\'':
        return parse_single_qs(data, pointer);
    case '"':
        return parse_double_qs(data, pointer);
    case '$':
        return parse_variable(data, pointer);
    case '|':
        pointer++;
        return std::make_optional(Token(Pipe, "|"));
        break;
    }

    return parse_word(data, --pointer);
}