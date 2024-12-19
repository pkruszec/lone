#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

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
    //
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

const char *token_type(Token_Type t)
{
    switch (t) {
        case TOKEN_EOF: return "eof";
        case TOKEN_SYM: return "sym";
        case TOKEN_NUM: return "num";

        case TOKEN_UNSET:
        case TOKEN_COUNT:
            return "<invalid>";
    }
    // TODO: Keywords, operators
    return "<invalid>";
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

    while ((c = lexer_advance(lex))) {
        switch (type) {
            case TOKEN_UNSET: {
            } break;
            default: {
                error_exit(lex->loc, "Unimplemented token type.");
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

    Lexer lex = {0};
    lex.loc.path = path;
    lex.src = src;
    lex.len = len;
     
    while (1) {
        Token tok = {0};
        lexer_next(&lex, &tok);
        printf("%s\n", token_type(tok.type));
        if (tok.type == TOKEN_EOF) break;
    }
     
    return 0;
}
