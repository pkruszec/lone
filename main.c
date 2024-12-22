#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "common.h"
#include "lexer.h"
#include "parser.h"

#include "common.c"
#include "lexer.c"
#include "parser.c"

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

    #if 0
    for (int i = 0; i < tokens.count; ++i) {
        Token *tok = &tokens.data[i];
        char buf[64] = {0};
        token_repr(buf, ARRAY_COUNT(buf) - 1, tok);
        printf("%s, %s\n", token_type(tok->type), buf);
    }
    #endif
    
    Parser parser = {0};
    parser.loc.path = lex.loc.path;
    parser.tokens = tokens.data;
    parser.token_count = tokens.count;

    Node *node = parser_parse_prog(&parser);
    print_node(node, 0);
    
    return 0;
}
