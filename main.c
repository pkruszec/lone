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

// Dynamic Array Helpers

#define append_many(a, c) (stretch_buffer(sizeof(*(a)->data), (void **)&(a)->data, &(a)->allocated, &(a)->count, (c)), &(a)->data[(a)->count - c])
#define append(a)         (append_many(a, 1))

void stretch_buffer(int sz, void **data, int *allocated, int *count, int add)
{
    if (*count + add > *allocated) {
        int min = *count + add - *allocated;
        *allocated += MAX(min, *allocated);
        *data = realloc(*data, *allocated * sz);
    }
    *count += add;
}

// String Builder

typedef struct {
    char *data;
    int allocated;
    int count;
} String_Builder;

char *sb_append(String_Builder *sb, char c)
{
    char *p = append_many(sb, 1);
    *p = c;
    return p;
}

// Lexer

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
    [TOKEN_MINUS] = "-",
    [TOKEN_ASTERISK] = "*",
    [TOKEN_MODULO] = "%",
    [TOKEN_SLASH] = "/",
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
            c == '\v'||
            c == '\0');
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

void token_repr(char *buf, int max, Token *tok)
{
    if (tok->type == TOKEN_EOF) {
        snprintf(buf, max, "EOF");
        return;
    }
    
    if (tok->type == TOKEN_SYM || tok->type == TOKEN_NUM) {
        snprintf(buf, max, "`%.*s`", (int)tok->len, tok->data);
        return;
    }
  
    // TODO: When reporting strings and characters, print all control and non-ASCII characters
    // as escape codes, in a way compatible with the source code.

    if (tok->type == TOKEN_STR) {
        snprintf(buf, max, "\"%.*s\"", (int)tok->len, tok->data);
        return;
    }

    if (tok->type == TOKEN_CHAR) {
        snprintf(buf, max, "'%.*s'", (int)tok->len, tok->data);
        return;
    }

    // Fallback.
    snprintf(buf, max, "`%s`", token_type(tok->type));
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
                if (c == '/' && n == '/') {
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

    while (1) {
        c = lexer_peek(lex);
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

        if (c == 0) break;
    }

    tok->loc = lex->loc;
    tok->type = TOKEN_EOF; 
}

// Parser

typedef enum {
    // Terminals
    NODE_IDENT,
    NODE_NUM,
    NODE_CHAR,
    NODE_STR,
    // Unary operators
    NODE_U_PLUS,
    NODE_U_MINUS,
    // Binary operators
    NODE_MUL,
    NODE_DIV,
    NODE_MOD,
    NODE_ADD,
    NODE_SUB,
    // Meta-types
    NODE_COUNT,
} Node_Type;

typedef struct Node Node;
typedef struct Node_Block Node_Block;

typedef struct {
    Node **data;
    int count;
    int allocated;
} Node_Children;

struct Node {
    Node_Type type;
    Node_Children children;
    char *data;
    int len;
};

struct Node_Block {
    Node_Block *next;
    Node data[8];
    int count;
};

typedef struct {
    Node_Block *head;
    Node_Block *last;
    int count;
} Node_Pool;

typedef struct {
    Token *tokens;
    int token_count;
    int token_pos;
    Node_Pool node_pool;
} Parser;

const char *node_type(Node_Type type)
{
    switch (type) {
        case NODE_IDENT: return "ident";
        case NODE_NUM: return "num";
        case NODE_CHAR: return "char";
        case NODE_STR: return "str";
        case NODE_U_PLUS: return "unary_plus";
        case NODE_U_MINUS: return "unary_minus";
        case NODE_MUL: return "mul";
        case NODE_DIV: return "div";
        case NODE_MOD: return "mod";
        case NODE_ADD: return "add";
        case NODE_SUB: return "sub";
        case NODE_COUNT: break;
    }
    return "<invalid>";
}

void node_append_child(Node *node, Node *child)
{
    Node **buf = append(&node->children);
    *buf = child;
}

Node_Block *node_pool_add_block(Node_Pool *pool)
{
    Node_Block *blk = malloc(sizeof(*blk));
    memset(blk, 0, sizeof(*blk));
    if (pool->last) {
        pool->last->next = blk;
        pool->last = pool->last->next;
    } else {
        pool->head = blk;
        pool->last = blk;
    }
    pool->count++;
    return pool->last;
}

Node *parser_alloc_node(Parser *parser)
{
    Node_Block *last = parser->node_pool.last;
    if (!last || last->count + 1 >= (int)ARRAY_COUNT(last->data)) {
        last = node_pool_add_block(&parser->node_pool);
    }
    Node *node = &last->data[last->count++];
    return node;
}

Token *parser_peek(Parser *parser)
{
    return &parser->tokens[parser->token_pos];
}

Token *parser_next(Parser *parser)
{
    Token *token = &parser->tokens[parser->token_pos];
    if (parser->token_pos < parser->token_count - 1) parser->token_pos++;
    return token;
}

void parser_expect_token_type_msg(Token *tok, Token_Type type, const char *expected)
{
    if (tok->type != type) {
        char buf[64] = {0};
        token_repr(buf, ARRAY_COUNT(buf) - 1, tok);
        error_exit(tok->loc, "Expected %s, got %s.\n", expected, buf);
    }
}

void parser_expect_token_type(Token *tok, Token_Type type)
{
    if (tok->type != type) {
        char buf[64] = {0};
        token_repr(buf, ARRAY_COUNT(buf) - 1, tok);
        error_exit(tok->loc, "Expected `%s`, got %s.\n", token_type(type), buf);
    }
}

Node *parser_parse_expr(Parser *parser);
Node *parser_parse_mul(Parser *parser);
Node *parser_parse_unary(Parser *parser);
Node *parser_parse_term(Parser *parser);

Node *parser_parse_expr(Parser *parser)
{
    return parser_parse_mul(parser);
}

Node *parser_parse_mul(Parser *parser)
{
    Node *base = parser_parse_unary(parser);

    while (1) {
        Token *tok = parser_peek(parser);
        Node *node = NULL;
        if (tok->type == TOKEN_ASTERISK) {
            parser_next(parser);
            node = parser_alloc_node(parser);
            node->type = NODE_MUL;
        } else if (tok->type == TOKEN_SLASH) {
            parser_next(parser);
            node = parser_alloc_node(parser);
            node->type = NODE_DIV;
        } else if (tok->type == TOKEN_MODULO) {
            parser_next(parser);
            node = parser_alloc_node(parser);
            node->type = NODE_MOD;
        }
        // info(tok->loc, "%s, %p", token_type(tok->type), node);
        if (!node) break;

        node_append_child(node, base);
        node_append_child(node, parser_parse_unary(parser));
        base = node;
    }
    
    return base;
}

Node *parser_parse_unary(Parser *parser)
{
    Node *first = NULL;
    Node *last = NULL;
    Node *base = NULL;

    while (1) {
        Token *tok = parser_peek(parser);
        Node *node = NULL;
        if (tok->type == TOKEN_PLUS) {
            parser_next(parser);
            node = parser_alloc_node(parser);
            node->type = NODE_U_PLUS;
        } else if (tok->type == TOKEN_MINUS) {
            parser_next(parser);
            node = parser_alloc_node(parser);
            node->type = NODE_U_MINUS;
        }
        if (!node) break;

        last = node;
        if (!base) {
            base = node;
            first = base;
        } else {
            node_append_child(base, node);
            base = node;
        }
    }

    Node *rest = parser_parse_term(parser);
    
    if (last && last) {
        node_append_child(last, rest);
        return first;
    }

    return rest;
}

Node *parser_parse_term(Parser *parser)
{
    Token *tok = parser_next(parser);

    // TODO: Make a lookup table for that
    if (tok->type == TOKEN_NUM) {
        Node *node = parser_alloc_node(parser);
        node->type = NODE_NUM;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    } else if (tok->type == TOKEN_CHAR) {
        Node *node = parser_alloc_node(parser);
        node->type = NODE_CHAR;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    } else if (tok->type == TOKEN_STR) {
        Node *node = parser_alloc_node(parser);
        node->type = NODE_STR;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    } else if (tok->type == TOKEN_SYM) {
        Node *node = parser_alloc_node(parser);
        node->type = NODE_IDENT;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    }

    const char *expected = "expression";
    parser_expect_token_type_msg(tok, TOKEN_POPEN, expected);
    Node *expr = parser_parse_expr(parser);
    tok = parser_next(parser);
    parser_expect_token_type_msg(tok, TOKEN_PCLOSE, expected);
    return expr;
}

// entry point

typedef struct {
    Token *data;
    int count;
    int allocated;
} Token_Array;

void print_node(Node *node, int indent)
{
    for (int i = 0; i < indent; ++i) fputc(' ', stderr);
    if (node == NULL) {
        fprintf(stderr, "<null>\n");
        return;
    }
    fprintf(stderr, "%s", node_type(node->type));

    if (node->len > 0) {
        fprintf(stderr, "(%.*s)", node->len, node->data);
    }

    fprintf(stderr, "\n");
    
    for (int i = 0; i < node->children.count; ++i) {
        Node *child = node->children.data[i];
        print_node(child, indent + 2);
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s <file>\n", argv[0]);
        fprintf(stderr, "error: no source file provided\n");
        return 1;
    }
    
    const char *path = argv[1];
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

    Token_Array tokens = {0};
    while (1) {
        Token *tok = append(&tokens);
        memset(tok, 0, sizeof(*tok));
        lexer_next(&lex, tok);
        if (tok->type == TOKEN_EOF) break;
    }

    Parser parser = {0};
    parser.tokens = tokens.data;
    parser.token_count = tokens.count;

    Node *node = parser_parse_expr(&parser);
    print_node(node, 0);
    
    #if 0
    for (int i = 0; i < tokens.count; ++i) {
        Token *tok = &tokens.data[i];
        char buf[64] = {0};
        token_repr(buf, ARRAY_COUNT(buf) - 1, tok);
        printf("%s, %s\n", token_type(tok->type), buf);
    }
    #endif
    
    return 0;
}
