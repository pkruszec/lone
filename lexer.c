#include "lexer.h"

static const char *operators[TOKEN_COUNT] = {
    [TOKEN_EQUAL] = "==",
    [TOKEN_PLUS] = "+",
    [TOKEN_MINUS] = "-",
    [TOKEN_ASTERISK] = "*",
    [TOKEN_MODULO] = "%",
    [TOKEN_SLASH] = "/",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_COLON] = ":",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_DOT] = ".",
    [TOKEN_COMMA] = ",",
    [TOKEN_POPEN] = "(",
    [TOKEN_PCLOSE] = ")",
    [TOKEN_COPEN] = "{",
    [TOKEN_CCLOSE] = "}",
    [TOKEN_SOPEN] = "[",
    [TOKEN_SCLOSE] = "]",
    [TOKEN_ARROW] = "->",
};

static const char *keywords[TOKEN_COUNT] = {
    [TOKEN_PROC] = "proc",
    [TOKEN_TYPE] = "type",
    [TOKEN_IF] = "if",
    [TOKEN_ELSE] = "else",
    [TOKEN_VAR] = "var",
};
