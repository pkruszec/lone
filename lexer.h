#ifndef LEXER_H
#define LEXER_H

typedef enum {
    TOKEN_EOF = 0,
    TOKEN_SYM,
    TOKEN_NUM,
    TOKEN_STR,
    TOKEN_CHAR,
    // Operators
    TOKEN_EQUAL,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_MODULO,
    TOKEN_SEMICOLON,
    TOKEN_ASSIGN,
    TOKEN_COLON,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_POPEN,
    TOKEN_PCLOSE,
    TOKEN_COPEN,
    TOKEN_CCLOSE,
    TOKEN_SOPEN,
    TOKEN_SCLOSE,
    TOKEN_ARROW,
    // Keywords
    TOKEN_PROC,
    TOKEN_TYPE,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_VAR,
    // Meta-tokens
    TOKEN_SOME_RAW,
    TOKEN_SOME_OP,
    TOKEN_UNSET,
    TOKEN_COUNT,
} Token_Type;

typedef struct {
    Location loc;
    Token_Type type;
    int len;
    char *data;
} Token;

typedef struct {
    char *src;
    int len;
    int pos;
    Location loc;
} Lexer;

const char *token_type(Token_Type t);
void token_repr(char *buf, int max, Token *tok);
void lexer_next(Lexer *lex, Token *tok);

#endif // LEXER_H