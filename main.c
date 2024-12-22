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
    for (int i = 0; i < indent; ++i) fputc(' ', stdout);
    if (node == NULL) {
        fprintf(stdout, "<null>\n");
        return;
    }
    fprintf(stdout, "%s", node_type(node->type));

    if (node->len > 0) {
        fprintf(stdout, "(%.*s)", node->len, node->data);
    }

    fprintf(stdout, "\n");
    
    for (int i = 0; i < node->children.count; ++i) {
        Node *child = node->children.data[i];
        print_node(child, indent + 2);
    }
}

typedef struct {
    int type;
    Node *name;
    Node *value;
} Typed_Constant;

typedef enum {
    TYPE_VOID = 0,
    TYPE_PTR,
    TYPE_BOOL,
    TYPE_S32,
    TYPE_FLOAT32,
} Type_Type;

typedef struct {
    Type_Type type;
    const char *name;
    int len;
    int ptr_to;
} Type;

typedef struct {
    Typed_Constant *data;
    int count;
    int allocated;
} Typed_Constants;

typedef struct {
    Type *data;
    int count;
    int allocated;
} Types;

typedef struct {
    Types types;
    Typed_Constants consts;
} Typed_Module;

void type_append_base(Typed_Module *mod, Type_Type tt, const char *name)
{
    Type *t = append_zero(&mod->types);
    t->type = tt;
    t->name = name;
    t->len = strlen(name);
}

int type_from_node(Node *node, Typed_Module *mod)
{
    switch (node->type) {
        case NODE_IDENT: {
            for (int i = 0; i < mod->types.count; ++i) {
                Type *type = &mod->types.data[i];
                if (type->len != node->len) continue;
                if (strncmp(type->name, node->data, type->len) == 0) {
                    return i;
                }
            }
        } break;
        case NODE_T_PTR: {
            assert(node->children.count >= 1);
            Node *ptr_to = node->children.data[0];
            int tfn = type_from_node(ptr_to, mod);

            for (int i = 0; i < mod->types.count; ++i) {
                Type *type = &mod->types.data[i];
                if (type->type != TYPE_PTR) continue;
                if (tfn != type->ptr_to) continue;

                return i;
            }

            Type *t = append_zero(&mod->types);
            t->type = TYPE_PTR;
            t->ptr_to = tfn;
            return mod->types.count - 1;
        } break;
        default: {
            error_exit(node->loc, "Syntactically incorrect type annotations should have been catched in the parsing stage. This is a compiler bug.");
        } break;
    }

    error_exit(node->loc, "Unknown type `%.*s`.", node->len, node->data);
    return 0;
}

void type_annotate(Node *node, Typed_Module *mod)
{
    Node **units = node->children.data;
    int count = node->children.count;

    type_append_base(mod, TYPE_VOID, "void");
    type_append_base(mod, TYPE_S32, "s32");
    type_append_base(mod, TYPE_FLOAT32, "float32");

    // TODO: Add all compiler-defined type aliases, like int
    // TODO: Fetch all user-defined types

    for (int i = 0; i < count; ++i) {
        Node *unit = units[i];
        
        switch (unit->type) {
            case NODE_CONST: {
                assert(unit->children.count >= 2);
                Node *name = unit->children.data[0];
                Node *value = unit->children.data[1];
                int type = 0;
                if (unit->children.count > 2) {
                    type = type_from_node(unit->children.data[2], mod);
                    info(name->loc, "%d", type);
                } else {
                    assert(false);
                }
                
            } break;
            default: {
                error_exit(unit->loc, "Invalid top-level statement.");
            } break;
        }
    }
}

void print_type(Type *type, Typed_Module *mod, int depth)
{
    if (type->type == TYPE_PTR) {
        fprintf(stderr, "*");
        print_type(&mod->types.data[type->ptr_to], mod, depth + 1);
    } else {
        fprintf(stderr, "%.*s", type->len, type->name);
    }

    if (depth == 0) fprintf(stderr, "\n");
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
        Token *tok = append_zero(&tokens);
        lexer_next(&lex, tok);
        if (tok->type == TOKEN_EOF) break;
    }
    
    Parser parser = {0};
    parser.loc.path = lex.loc.path;
    parser.tokens = tokens.data;
    parser.token_count = tokens.count;

    Node *node = parser_parse_prog(&parser);
    print_node(node, 0);
    Typed_Module mod = {0};
    type_annotate(node, &mod);

    for (int i = 0; i < mod.types.count; ++i) {
        Type *type = &mod.types.data[i];
        fprintf(stderr, "%d: ", i);
        print_type(type, &mod, 0);
    }
    
    return 0;
}
