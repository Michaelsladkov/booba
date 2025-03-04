#pragma once
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <vector>

enum TokenType : uint8_t { Word, SingleQS, DoubleQS, Variable, Pipe, Path };

class Token {

  public:
    const TokenType type;
    const std::string content;
    const std::optional<std::vector<size_t>> variables_positions;

    Token(TokenType type, std::string &content)
        : type(type), content(content), variables_positions(std::nullopt) {}

    Token(TokenType type, std::string &&content)
        : type(type), content(std::move(content)),
          variables_positions(std::nullopt) {}

    Token(TokenType type, std::string &content,
          std::vector<size_t> &variables_positions)
        : type(type), content(content),
          variables_positions(std::make_optional(variables_positions)) {}

    Token(TokenType type, std::string &&content,
          std::vector<size_t> &variables_positions)
        : type(type), content(std::move(content)),
          variables_positions(std::make_optional(variables_positions)) {}

    Token(TokenType type, std::string &content,
          std::vector<size_t> &&variables_positions)
        : type(type), content(content), variables_positions(std::make_optional(
                                            std::move(variables_positions))) {}

    Token(TokenType type, std::string &&content,
          std::vector<size_t> &&variables_positions)
        : type(type), content(std::move(content)),
          variables_positions(
              std::make_optional(std::move(variables_positions))) {}
};