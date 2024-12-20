#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define ARRAY_COUNT(a) (sizeof(a) / sizeof(a[0]))

char *read_file(const char *path, int *len)
{
    FILE *f = fopen(path, "rb");
    if (f == NULL) return NULL;

    fseek(f, 0, SEEK_END);
    int sz = ftell(f);
    fseek(f, 0, SEEK_SET);
   
    char *buf = malloc(sz + 1);
    buf[sz] = '\0';
    int nr = fread(buf, 1, sz, f);
    
    if (nr != sz) {
        buf = NULL;
        sz = 0;
    }
 
    if (len) *len = sz;
    fclose(f); 
    return buf;
}

typedef struct {
    const char *path;
    int line;
    int col;
} Location;

void msg(const char *mode, Location loc, const char *fmt, ...)
{
    if (loc.path)
        fprintf(stderr, "%s:%d:%d: ", loc.path, loc.line + 1, loc.col + 1);
    else
        fprintf(stderr, "%d:%d: ", loc.line + 1, loc.col + 1);

    fprintf(stderr, "%s: ", mode);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
}

#define info(...) msg("info", __VA_ARGS__)
#define error(...) msg("error", __VA_ARGS__)
#define warn(...) msg("warning", __VA_ARGS__)
#define error_exit(...) do {error(__VA_ARGS__); exit(1);} while (0)

// String Builder

typedef struct {
    char *data;
    int allocated;
    int count;
} String_Builder;

char *sb_append_many(String_Builder *sb, int count)
{
    if (sb->count + count >= sb->allocated) {
        int min = sb->count + count - sb->allocated;
        sb->allocated += MAX(min, sb->allocated);
        sb->data = realloc(sb->data, sb->allocated * sizeof(*sb->data));
    }
    char *data = sb->data + sb->count;
    sb->count += count;
    return data;
}

char *sb_append(String_Builder *sb, char c)
{
    char *p = sb_append_many(sb, 1);
    *p = c;
    return p;
}

// Lexer definitions

typedef enum {
    TOKEN_EOF = 0,
    TOKEN_SYM,
    TOKEN_NUM,
    TOKEN_STR,
    TOKEN_CHAR,
    // Operators
    TOKEN_EQUAL,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_ASSIGN,
    TOKEN_COLON,
    TOKEN_DOT,
    TOKEN_POPEN,
    TOKEN_PCLOSE,
    TOKEN_COPEN,
    TOKEN_CCLOSE,
    TOKEN_SOPEN,
    TOKEN_SCLOSE,
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

static const char *operators[TOKEN_COUNT] = {
    [TOKEN_EQUAL] = "==",
    [TOKEN_PLUS] = "+",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_COLON] = ":",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_DOT] = ".",
    [TOKEN_POPEN] = "(",
    [TOKEN_PCLOSE] = ")",
    [TOKEN_COPEN] = "{",
    [TOKEN_CCLOSE] = "}",
    [TOKEN_SOPEN] = "[",
    [TOKEN_SCLOSE] = "]",
};

static const char *keywords[TOKEN_COUNT] = {
    [TOKEN_PROC] = "proc",
    [TOKEN_TYPE] = "type",
    [TOKEN_IF] = "if",
    [TOKEN_ELSE] = "else",
    [TOKEN_VAR] = "var",
};

bool is_white_space(char c)
{
    return (c == ' ' ||
            c == '\n'||
            c == '\r'||
            c == '\v');
}

bool is_end_of_line(char c)
{
    return c == '\n' || c == '\r';
}

bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

bool is_operator(char *str, int count, char c)
{
    for (int i = 0; i < (int)ARRAY_COUNT(operators); ++i) {
        const char *op = operators[i];
        if (op == NULL) continue;
        if ((int)strlen(op) < count+1) continue;
        if (strncmp(str, op, count) != 0) continue;
        if (op[count] == c) return true;
    }
    return false;
}

bool is_symbol(char *str, int count, char c)
{
    (void)str;
    (void)count;
    if (c == '/') return false;
    if (is_white_space(c)) return false;
    if (is_operator(0, 0, c)) return false;
    return true;
}

Token_Type operator_from_str(char *str, int count)
{
    for (int i = 0; i < (int)ARRAY_COUNT(operators); ++i) {
        const char *op = operators[i];
        if (op == NULL) continue;
        if ((int)strlen(op) != count) continue;
        if (strncmp(str, op, count) != 0) continue;
        return (Token_Type)i;
    }
    return TOKEN_EOF;
}

Token_Type keyword_from_str(char *str, int count)
{
    for (int i = 0; i < (int)ARRAY_COUNT(keywords); ++i) {
        const char *kwd = keywords[i];
        if (kwd == NULL) continue;
        if ((int)strlen(kwd) != count) continue;
        if (strncmp(str, kwd, count) != 0) continue;
        return (Token_Type)i;
    }
    return TOKEN_EOF;
}

const char *token_type(Token_Type t)
{
    switch (t) {
        case TOKEN_EOF: return "eof";
        case TOKEN_SYM: return "sym";
        case TOKEN_NUM: return "num";
        case TOKEN_STR: return "str";
        case TOKEN_CHAR: return "char";
        case TOKEN_UNSET: return "unset";
        case TOKEN_SOME_OP: return "some_op";
        case TOKEN_SOME_RAW: return "some_raw";
        default:  break;
    }
   
    if (operators[t] != NULL) return operators[t];
    if (keywords[t] != NULL) return keywords[t];

    return "<invalid>";
}

void token_repr(char *buf, int max, Token tok)
{
    if (tok.type == TOKEN_SYM || tok.type == TOKEN_NUM) {
        snprintf(buf, max, "`%.*s`", (int)tok.len, tok.data);
        return;
    }
  
    // TODO: When reporting strings and characters, print all control and non-ASCII characters
    // as escape codes, in a way compatible with the source code.

    if (tok.type == TOKEN_STR) {
        snprintf(buf, max, "\"%.*s\"", (int)tok.len, tok.data);
        return;
    }

    if (tok.type == TOKEN_CHAR) {
        snprintf(buf, max, "'%.*s'", (int)tok.len, tok.data);
        return;
    }

    // Fallback.
    snprintf(buf, max, "`%s`", token_type(tok.type));
}

char lexer_advance(Lexer *lex)
{
    // TODO: This function adjusts column by bytes, which is incorrect when encountering non-ASCII characters
    if (lex->pos >= lex->len - 1) return 0;
    char c = lex->src[lex->pos];
    lex->pos++;
    if (c == '\n') {
        lex->loc.line++;
        lex->loc.col = 0;
    } else {
        lex->loc.col++;
    }
    return c;
}

char lexer_peek(Lexer *lex)
{
    if (lex->pos >= lex->len - 1) return 0;
    return lex->src[lex->pos];
}

char lexer_peek2(Lexer *lex)
{
    if (lex->pos >= lex->len - 2) return 0;
    return lex->src[lex->pos + 1];
}

char lexer_escape(Lexer *lex)
{
    char c = lexer_peek(lex);
    if (c == '\'') {
        lexer_advance(lex);
        return '\'';
    } else if (c == '\"') {
        lexer_advance(lex);
        return '\"';
    } else if (c == 'n') {
        lexer_advance(lex);
        return '\n';
    }
    // TODO: Support everything C supports
    return 0;
}

void lexer_next(Lexer *lex, Token *tok)
{
    Location block_loc = lex->loc;
    bool comment = false;
    int block = 0;
    char c;

    // Skip all whitespace and comments.
    // Handles both // single line
    // and nested /* multiline */ comments.
    // Line comments are terminated with \r or \n.
    while ((c = lexer_peek(lex))) {
        bool got_block = false;
        char n = lexer_peek2(lex);
        if (c == '/' && n == '*') {
            // We save the position of the comment start
            // so that we can print it in case the user didn't
            // close the comment. 
            block_loc = lex->loc;
            lexer_advance(lex);
            block++;
            got_block = true;
        }
        if (c == '*' && n == '/') {
            if (block == 0) {
                error_exit(lex->loc, "Unexpected block comment end.");
            }
            lexer_advance(lex);
            block--;
            got_block = true;
        }
        
        if (!got_block) {
            if (comment && (is_end_of_line(c))) {
                comment = false;
            } else if (!comment) {
                if (c == '/') {
                    if (n == '/')
                        comment = true;
                } else if (block == 0 && !is_white_space(c)) {
                    break;
                }
            }
        }

        lexer_advance(lex);
    }

    if (block > 0) {
        error_exit(block_loc, "Unterminated block comment.");
    }

    Token_Type type = TOKEN_UNSET;
    String_Builder sb = {0};
    int index = 0;
    Location token_loc = lex->loc;

    while ((c = lexer_peek(lex))) {
        switch (type) {
            case TOKEN_UNSET: {
                sb.count = 0;
                index = 0;
                token_loc = lex->loc;
                char n = lexer_peek2(lex);
              
                // TODO: Raw Strings
                // ``test``
                // `GLSL`// `funcer`test`GLSL`
                //

                if (c == '\'') {
                    type = TOKEN_CHAR;
                    lexer_advance(lex);
                } else if (c == '\"') {
                    type = TOKEN_STR;
                    lexer_advance(lex);
                } else if (is_digit(c) || (c == '.' && is_digit(n))) {
                    type = TOKEN_NUM;
                } else if (is_operator(0, 0, c)) {
                    type = TOKEN_SOME_OP;
                } else {
                    type = TOKEN_SYM;
                }
            } break;
            case TOKEN_STR: {
                if (c == '\\') {
                    lexer_advance(lex);
                    char e = lexer_escape(lex);
                    if (e == 0) {
                        char bad = lexer_peek(lex);
                        if (bad != 0)
                            error_exit(token_loc, "Unrecognized escape sequence `\\%c`.", bad);
                        else
                            error_exit(token_loc, "Expected an escape sequence, got EOF.");
                    }
                    sb_append(&sb, e);
                } else if (c == '\"') {
                    tok->type = type;
                    tok->len = sb.count;
                    tok->data = sb.data;
                    lexer_advance(lex);
                    return;
                } else {
                    sb_append(&sb, c);
                    lexer_advance(lex);
                }
            } break;
            case TOKEN_CHAR: {
                if (c == '\\') {
                    lexer_advance(lex);
                    char e = lexer_escape(lex);
                    if (e == 0) {
                        char bad = lexer_peek(lex);
                        if (bad != 0)
                            error_exit(token_loc, "Unrecognized escape sequence `\\%c`.", bad);
                        else
                            error_exit(token_loc, "Expected an escape sequence, got EOF.");
                    }
                    sb_append(&sb, e);
                } else if (c == '\'') {
                    tok->type = type;
                    tok->len = sb.count;
                    tok->data = sb.data;
                    lexer_advance(lex);
                    return;
                } else {
                    sb_append(&sb, c);
                    lexer_advance(lex);
                }
            } break;
            case TOKEN_SOME_OP: {
                if (is_operator(sb.data, sb.count, c)) {
                    sb_append(&sb, c);
                    lexer_advance(lex);
                } else {
                    tok->loc = token_loc;
                    tok->type = operator_from_str(sb.data, sb.count);
                    if (tok->type == TOKEN_EOF) {
                        error_exit(token_loc, "Unrecognized operator: '%.*s'.", sb.count, sb.data);
                    }
                    return;
                }
                index++;
            } break;
            case TOKEN_SYM: {
                if (is_symbol(sb.data, sb.count, c)) {
                    sb_append(&sb, c);
                    lexer_advance(lex);
                } else {
                    tok->loc = token_loc;
                    Token_Type kwd = keyword_from_str(sb.data, sb.count);
                    if (kwd != TOKEN_EOF) {
                        tok->type = kwd;
                        return;
                    }

                    tok->type = TOKEN_SYM;
                    tok->len = sb.count;
                    tok->data = sb.data;
                    return;
                }
            } break;
            case TOKEN_NUM: {
                // We use is_symbol here, because at the lexing stage we don't care about the correctness
                // of the number, we just want to pass it down.
                if (c == '.' || is_symbol(sb.data, sb.count, c)) {
                    sb_append(&sb, c);
                    lexer_advance(lex);
                } else {
                    tok->loc = token_loc;
                    tok->type = type;
                    tok->len = sb.count;
                    tok->data = sb.data;
                    return;
                }
            } break;
            default: {
                error_exit(lex->loc, "Unimplemented token type `%s`", token_type(type));
            } break;
        }
    }

    tok->type = TOKEN_EOF; 
}

int main(void)
{
    const char *path = "test.ln";
    int len = 0;
    char *src = read_file(path, &len);
    if (src == NULL) {
        fprintf(stderr, "error: Could not open file %s: %s\n", path, strerror(errno));
        return 1;
    }

    Lexer lex = {0};
    lex.loc.path = path;
    lex.src = src;
    lex.len = len;
     
    while (1) {
        Token tok = {0};
        lexer_next(&lex, &tok);
        char buf[64] = {0};
        token_repr(buf, ARRAY_COUNT(buf) - 1, tok);
        printf("%s(%s)\n", token_type(tok.type), buf); 
        if (tok.type == TOKEN_EOF) break;
    }
     
    return 0;
}

