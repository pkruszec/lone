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
    [TOKEN_RETURN] = "return",
};

static const char *directives[TOKEN_COUNT] = {
    [TOKEN_DIR_IF] = "#if",
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

Token_Type directive_from_str(char *str, int count)
{
    for (int i = 0; i < (int)ARRAY_COUNT(directives); ++i) {
        const char *dir = directives[i];
        if (dir == NULL) continue;
        if ((int)strlen(dir) != count) continue;
        if (strncmp(str, dir, count) != 0) continue;
        return (Token_Type)i;
    }
    return TOKEN_EOF;
}

const char *token_type(Token_Type t)
{
    switch (t) {
        case TOKEN_EOF: return "eof";
        case TOKEN_SYM: return "sym";
        case TOKEN_DIRECTIVE: return "directive";
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
    if (directives[t] != NULL) return directives[t];

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
                } else if (c == '#') {
                    type = TOKEN_DIRECTIVE;
                    lexer_advance(lex);
                    sb_append(&sb, c);
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
            case TOKEN_DIRECTIVE: {
                if (is_symbol(sb.data, sb.count, c)) {
                    sb_append(&sb, c);
                    lexer_advance(lex);
                } else {
                    tok->loc = token_loc;
                    Token_Type dir = directive_from_str(sb.data, sb.count);
                    if (dir != TOKEN_EOF) {
                        tok->type = dir;
                        return;
                    }

                    error_exit(token_loc, "Unrecognized directive: '%.*s'.", sb.count, sb.data);
                    return;
                }
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
