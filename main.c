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

typedef enum {
    TYPE_VOID = 0,
    TYPE_PTR,
    TYPE_COMPTIME_INT,
    TYPE_COMPTIME_FLOAT,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FLOAT,
    // TYPE_S8,
    // TYPE_S16,
    // TYPE_S32,
    // TYPE_S64,
    // TYPE_U8,
    // TYPE_U16,
    // TYPE_U32,
    // TYPE_U64,
    // TYPE_FLOAT32,
    // TYPE_FLOAT64,
} Type_Type;

typedef struct {
    Type_Type type;
    const char *name;
    int len;
    union {
        struct {
            int to;
        } p;
        struct {
            bool sign;
            int size;
        } i;
        struct {
            bool dbl;
        } f;
    };
} Type;

typedef struct {
    int type;
    Node *name;
    Node *value;
} Typed_Constant;

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

Typed_Constant *mod_global_const_from_name(const char *name, int len, Typed_Module *mod)
{
    for (int i = 0; i < mod->consts.count; ++i) {
        Typed_Constant *c = &mod->consts.data[i];
        if (c->name->len != len) continue;
        if (strncmp(name, c->name->data, len)) continue;
        return c;
    }
    return NULL;
}

Type *type_append_base(Typed_Module *mod, Type_Type tt, const char *name)
{
    Type *t = append_zero(&mod->types);
    t->type = tt;
    t->name = name;
    t->len = strlen(name);
    return t;
}

Type *type_append_int(Typed_Module *mod, const char *name, bool sign, int size)
{
    Type *t = type_append_base(mod, TYPE_INT, name);
    t->i.sign = sign;
    t->i.size = size;
    return t;
}

Type *type_append_float(Typed_Module *mod, const char *name, bool dbl)
{
    Type *t = type_append_base(mod, TYPE_FLOAT, name);
    t->f.dbl = dbl;
    return t;
}

void type_repr(char *buf, int max, int type, Typed_Module *mod)
{
    Type *t = &mod->types.data[type];
    int len = 0;

    #define P(x)                        \
        do {                            \
            if (len + 1 >= max) return; \
            buf[len++] = (x);           \
        } while (0)

    if (t->type == TYPE_PTR) {
        P('*');
        type_repr(buf+len, max-len, t->p.to, mod);
    } else {
        for (int i = 0; i < t->len; ++i) {
            P(t->name[i]);
        }
    }

    #undef P
}

int type_base_from_type(Type_Type tt, Typed_Module *mod)
{
    for (int i = 0; i < mod->types.count; ++i) {
        if (mod->types.data[i].type == tt) return i;
    }

    assert(false);
    return 0;
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
                if (tfn != type->p.to) continue;

                return i;
            }

            Type *t = append_zero(&mod->types);
            t->type = TYPE_PTR;
            t->p.to = tfn;
            return mod->types.count - 1;
        } break;
        default: {
            error_exit(node->loc, "Syntactically incorrect type annotations should have been catched in the parsing stage. This is a compiler bug.");
        } break;
    }

    error_exit(node->loc, "Unknown type `%.*s`.", node->len, node->data);
    return 0;
}

bool type_can_cast_explicit(int from, int to, Typed_Module *mod)
{
    if (from == to) return true;
    Type *src = &mod->types.data[from];
    Type *dst = &mod->types.data[to];

    if (src->type == TYPE_COMPTIME_INT) {
        if (dst->type == TYPE_INT) return true;
        if (dst->type == TYPE_FLOAT) return true;
        if (dst->type == TYPE_PTR) return true;
    }

    if (src->type == TYPE_COMPTIME_FLOAT) {
        if (dst->type == TYPE_INT) return true;
        if (dst->type == TYPE_FLOAT) return true;
    }

    if (src->type == TYPE_INT) {
        if (dst->type == TYPE_PTR && !src->i.sign) return true;
        if (dst->type == TYPE_INT) return true;
        if (dst->type == TYPE_FLOAT) return true;
    }

    if (src->type == TYPE_FLOAT) {
        if (dst->type == TYPE_INT) return true;
        if (dst->type == TYPE_FLOAT) return true;
    }

    if (src->type == TYPE_PTR) {
        if (dst->type == TYPE_INT) return true;
    }

    return false;
}

bool type_can_cast_implicit(int from, int to, Typed_Module *mod)
{
    if (from == to) return true;
    Type *src = &mod->types.data[from];
    Type *dst = &mod->types.data[to];

    if (src->type == TYPE_INT) {
        if (dst->type != TYPE_INT) return false;
        if (dst->i.size < src->i.size) return false;
    }

    if (src->type == TYPE_FLOAT) {
        if (dst->type != TYPE_FLOAT && src->f.dbl != dst->f.dbl) return false;
    }

    return type_can_cast_explicit(from, to, mod);
}

int type_infer_literal(Node *node, Typed_Module *mod)
{
    switch (node->type) {
        case NODE_NUM: {
            bool has_point = false;
            for (int i = 0; i < node->len; ++i) {
                if (node->data[i] == '.') {
                    has_point = true;
                    break;
                }
            }

            if (has_point)
                return type_base_from_type(TYPE_COMPTIME_FLOAT, mod);
            else
                return type_base_from_type(TYPE_COMPTIME_INT, mod);
        };
        default: break;
    }

    error_exit(node->loc, "Unimplemented literal.");
    return 0;
}

int type_infer_constant(Node *node, Typed_Module *mod)
{
    switch (node->type) {
        case NODE_CHAR:
        case NODE_STR:
        case NODE_NUM: {
            return type_infer_literal(node, mod);
        } break;

        case NODE_IDENT: {
            char *name = node->data;
            int len = node->len;

            Typed_Constant *c = mod_global_const_from_name(name, len, mod);
            if (c) return c->type;

            error_exit(node->loc, "Undeclared identifier `%.*s`. Keep in mind that constants must be defined higher than their usage.", len, name);
        } break;

        case NODE_CAST: {
            assert(node->children.count >= 2);
            int casting_type = type_from_node(node->children.data[0], mod);
            int casted_type = type_infer_constant(node->children.data[1], mod);

            if (type_can_cast_explicit(casted_type, casting_type, mod)) {
                return casting_type;
            }

            char from_buf[64] = {0};
            char to_buf[64] = {0};
            type_repr(from_buf, ARRAY_COUNT(from_buf) - 1, casted_type, mod);
            type_repr(to_buf, ARRAY_COUNT(to_buf) - 1, casting_type, mod);

            error_exit(node->loc, "Cannot cast %s to %s.", from_buf, to_buf);
        } break;
        default: break;
    }

    error_exit(node->loc, "Syntactically incorrect expressions should have been catched in the parsing stage. This is a compiler bug.");
    return 0;
}

void type_annotate(Node *node, Typed_Module *mod)
{
    Node **units = node->children.data;
    int count = node->children.count;

    type_append_base(mod, TYPE_VOID, "void");
    type_append_base(mod, TYPE_COMPTIME_INT, "comptime_int");
    type_append_base(mod, TYPE_COMPTIME_FLOAT, "comptime_float");
    type_append_base(mod, TYPE_BOOL, "bool");

    type_append_int(mod, "s8", true, 1);
    type_append_int(mod, "s16", true, 2);
    type_append_int(mod, "s32", true, 4);
    type_append_int(mod, "s64", true, 8);
    type_append_int(mod, "u8", false, 1);
    type_append_int(mod, "u16", false, 2);
    type_append_int(mod, "u32", false, 4);
    type_append_int(mod, "u64", false, 8);
    type_append_float(mod, "float32", false);
    type_append_float(mod, "float64", true);

    // TODO: Add all compiler-defined type aliases, like int
    // TODO: Fetch all user-defined types

    for (int i = 0; i < count; ++i) {
        Node *unit = units[i];
        
        switch (unit->type) {
            case NODE_CONST: {
                assert(unit->children.count >= 2);
                Node *name = unit->children.data[0];
                Node *value = unit->children.data[1];
                assert(name->len > 0);

                Typed_Constant *c = mod_global_const_from_name(name->data, name->len, mod);
                if (c) {
                    error(name->loc, "Redefinition of `%.*s`.", name->len, name->data);
                    error_exit(c->name->loc, "The other defintion is here.");
                }

                Typed_Constant *tc = append_zero(&mod->consts);
                tc->name = name;
                tc->value = value;

                if (unit->children.count > 2) {
                    int type = type_from_node(unit->children.data[2], mod);
                    int inferred = type_infer_constant(value, mod);

                    if (!type_can_cast_implicit(inferred, type, mod)) {
                        char from_buf[64] = {0};
                        char to_buf[64] = {0};
                        type_repr(from_buf, ARRAY_COUNT(from_buf) - 1, inferred, mod);
                        type_repr(to_buf, ARRAY_COUNT(to_buf) - 1, type, mod);
                        error_exit(value->loc, "Cannot implicitly cast from %s to %s. Try to use an explicit cast: `cast(%s) value`", from_buf, to_buf, to_buf);
                    }

                    tc->type = type;
                } else {
                    int inferred = type_infer_constant(value, mod);
                    tc->type = inferred;
                }
            } break;
            // case NODE_VAR: {
            // } break;
            default: {
                error_exit(unit->loc, "Invalid top-level statement.");
            } break;
        }
    }
}

void print_type(int type, Typed_Module *mod)
{
    char buf[64] = {0};
    type_repr(buf, ARRAY_COUNT(buf) - 1, type, mod);
    fprintf(stderr, "%s\n", buf);
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
    Typed_Module mod = {0};
    type_annotate(node, &mod);

    #if 0
    // Types
    for (int i = 0; i < mod.types.count; ++i) {
        fprintf(stderr, "%d: ", i);
        print_type(i, &mod);
    }
    #endif

    #if 1
    // Global Constants
    for (int i = 0; i < mod.consts.count; ++i) {
        Typed_Constant *c = &mod.consts.data[i];
        fprintf(stderr, "%.*s: ", c->name->len, c->name->data);
        print_type(c->type, &mod);
    }
    #endif
    
    return 0;
}
