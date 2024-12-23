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
} Type_Type;

typedef enum {
    UNIT_NIL = 0,
    UNIT_CONSTANT,
    UNIT_VARIABLE,
    UNIT_PROC,
} Unit_Type;

typedef struct {
    Type_Type type;
    const char *name;
    int len;
    bool builtin;
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
    Type *data;
    int count;
    int allocated;
} Types;

typedef struct {
    const char *name;
    int len;
    int type;
} Proc_Arg;

typedef struct {
    const char *name;
    int len;
    int type;
    Node *value;
    int scope;
} Proc_Local;

typedef struct {
    Proc_Local *data;
    int count;
    int allocated;
} Proc_Locals;

typedef struct {
    Proc_Locals locals;
    Proc_Locals consts;
    struct {
        Proc_Arg *data;
        int count;
        int allocated;
    } args;

    Ints return_types;
    Ints scope_parents;
} Proc;

typedef struct {
    Unit_Type type;
    Node *node;
    const char *name;
    int len;
    bool type_checked;
    Ints dependencies;
    union {
        Proc proc;
    };
} Unit;

typedef struct {
    Unit *data;
    int count;
    int allocated;
} Units;

typedef struct {
    Types types;
    Units units;
} Typed_Module;

const char *unit_type(Unit_Type type)
{
    switch (type) {
        case UNIT_NIL: return "nil";
        case UNIT_CONSTANT: return "constant";
        case UNIT_VARIABLE: return "variable";
        case UNIT_PROC: return "proc";
    }
    return "<invalid>";
}

Type *type_append_base(Typed_Module *mod, Type_Type tt, const char *name)
{
    Type *t = append_zero(&mod->types);
    t->type = tt;
    t->name = name;
    t->len = strlen(name);
    t->builtin = true;
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

#if 0
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

int type_base_from_type(Type_Type tt, Typed_Module *mod)
{
    for (int i = 0; i < mod->types.count; ++i) {
        if (mod->types.data[i].type == tt) return i;
    }

    assert(false);
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
#endif

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

Unit *unit_ref(int id, Typed_Module *mod)
{
    return &mod->units.data[id];
}

int arg_from_name(const char *name, int len, Typed_Module *mod, int proc_id)
{
    if (!proc_id) return 0;
    Proc *proc = &unit_ref(proc_id, mod)->proc;
    for (int i = 0; i < proc->args.count; ++i) {
        Proc_Arg *item = &proc->args.data[i];
        if (item->len != len) continue;
        if (strncmp(item->name, name, len) != 0) continue;
        return i;
    }

    return 0;
}

bool scope_is_child(Typed_Module *mod, int proc_id, int scope, int parent)
{
    if (scope == parent) return true;
    Proc *proc = &unit_ref(proc_id, mod)->proc;

    int c = scope;
    while (true) {
        int p = proc->scope_parents.data[c];
        if (p == parent) return true;
        c = p;
        if (c == 0) break;
    }

    return false;
}

int local_from_name(const char *name, int len, Typed_Module *mod, int proc_id, int scope, bool do_constants)
{
    if (!proc_id) return 0;
    Proc *proc = &unit_ref(proc_id, mod)->proc;

    Proc_Locals *array = do_constants ? &proc->locals : &proc->consts;

    for (int i = 0; i < array->count; ++i) {
        Proc_Local *item = &array->data[i];
        if (item->len != len) continue;
        if (strncmp(item->name, name, len) != 0) continue;
        if (!scope_is_child(mod, proc_id, scope, item->scope)) continue;
        return i;
    }

    return 0;
}

int unit_from_name(const char *name, int len, Typed_Module *mod)
{
    for (int i = 0; i < mod->units.count; ++i) {
        Unit *u = &mod->units.data[i];
        if (u->len != len) continue;
        if (strncmp(u->name, name, len) != 0) continue;
        return i;
    }
    return 0;
}

int scope_new(int proc_id, int parent, Typed_Module *mod)
{
    assert(proc_id != 0);
    Proc *proc = &unit_ref(proc_id, mod)->proc;
    int *buf = append(&proc->scope_parents);
    *buf = parent;
    return proc->scope_parents.count - 1;
}

void process_unit(int id, Node *node, Typed_Module *mod, Node *parent)
{
    Unit *u = unit_ref(id, mod);
    if (!node) node = u->node;

    if (!parent) {
        switch (node->type) {
            case NODE_CONST:
            case NODE_VAR: {
                Node *value = node->children.data[1];
                process_unit(id, value, mod, node);
            } break;
            case NODE_PROC: {
                node->proc_id = id;
                for (int i = 0; i < node->children.count; ++i) {
                    node->children.data[i]->proc_id = node->proc_id;
                }

                append_zero(&u->proc.args);
                append_zero(&u->proc.locals);
                append_zero(&u->proc.consts);
                append_zero(&u->proc.scope_parents);

                Node *call = node->children.data[0];
                Node *ret_type = NULL;
                Node *body = NULL;

                if (node->children.count >= 3) {
                    ret_type = node->children.data[1];
                    body = node->children.data[2];
                } else if (node->children.count >= 2) {
                    Node *snd = node->children.data[1];
                    if (snd->type == NODE_PROC_RETVAL) {
                        ret_type = snd;
                    } else if (snd->type == NODE_PROC_BODY) {
                        body = snd;
                    } else {
                        assert(false);
                    }
                }

                (void)ret_type;

                for (int i = 1; i < call->children.count; ++i) {
                    Node *an = call->children.data[i];
                    Node *name = NULL;
                    Node *type = NULL;

                    if (an->children.count >= 2) {
                        name = an->children.data[0];
                        type = an->children.data[1];
                    } else {
                        type = an->children.data[0];
                    }

                    if (name->type != NODE_IDENT) {
                        error_exit(name->loc, "Argument name must be an identifier.");
                    }

                    Proc_Arg *arg = append_zero(&u->proc.args);
                    arg->type = type_from_node(type, mod);
                    if (name) {
                        arg->name = name->data;
                        arg->len = name->len;
                    }
                }

                if (body) {
                    body->scope = scope_new(body->proc_id, node->scope, mod);
                    process_unit(id, body, mod, node);
                }
            } break;
            default: {
                error_exit(node->loc, "Invalid top-level statement.");
            } break;
        }

        return;
    }

    if (node->proc_id) {
        for (int i = 0; i < node->children.count; ++i) {
            node->children.data[i]->proc_id = node->proc_id;
        }
    }
    if (node->scope) {
        for (int i = 0; i < node->children.count; ++i) {
            node->children.data[i]->scope = node->scope;
        }
    }

    switch (node->type) {
        case NODE_EVAL:
        case NODE_ASSIGN:
        case NODE_RETURN:
        case NODE_IF_COND: {
            for (int i = 0; i < node->children.count; ++i) {
                process_unit(id, node->children.data[i], mod, node);
            }
        }

        case NODE_PROC_CALL: {
            process_unit(id, node->children.data[0], mod, node);

            for (int i = 1; i < node->children.count; ++i) {
                Node *arg = node->children.data[i];

                for (int i = 0; i < arg->children.count; ++i) {
                    arg->children.data[i]->proc_id = arg->proc_id;
                    arg->children.data[i]->scope = arg->scope;
                }

                if (arg->children.count >= 2) {
                    process_unit(id, arg->children.data[1], mod, arg);
                } else {
                    process_unit(id, arg->children.data[0], mod, arg);
                }
            }
        } break;

        case NODE_IF_IF:
        case NODE_IF_ELSE:
        case NODE_PROC_BODY:
        case NODE_BLOCK: {
            int scope = scope_new(node->proc_id, node->scope, mod);
            for (int i = 0; i < node->children.count; ++i) {
                node->children.data[i]->scope = scope;
                process_unit(id, node->children.data[i], mod, node);
            }
        } break;

        case NODE_IF: {
            assert(node->children.count >= 1);

            for (int i = 0; i < node->children.count; ++i) {
                process_unit(id, node->children.data[i], mod, node);
            }
        } break;

        case NODE_VAR: {
            Node *name = node->children.data[0];
            Node *value = node->children.data[1];
            assert(node->proc_id);
            // TODO: Types

            Proc *proc = &unit_ref(node->proc_id, mod)->proc;
            Proc_Local *l = append_zero(&proc->locals);
            l->name = name->data;
            l->len = name->len;
            l->value = value;
            l->scope = node->scope;

            process_unit(id, value, mod, node);
        } break;

        case NODE_CONST: {
            Node *name = node->children.data[0];
            Node *value = node->children.data[1];
            assert(node->proc_id);
            // TODO: Types

            Proc *proc = &unit_ref(node->proc_id, mod)->proc;
            Proc_Local *l = append_zero(&proc->consts);
            l->name = name->data;
            l->len = name->len;
            l->value = value;
            l->scope = node->scope;

            process_unit(id, value, mod, node);
        } break;

        case NODE_IDENT: {
            int dep = 0;

            int arg = arg_from_name(node->data, node->len, mod, node->proc_id);
            int loc = local_from_name(node->data, node->len, mod, node->proc_id, node->scope, false);
            int con = local_from_name(node->data, node->len, mod, node->proc_id, node->scope, true);
            if ((!!arg + !!loc + !!con) > 1) {
                assert(false);
            }

            if (arg || loc || con) break;

            dep = unit_from_name(node->data, node->len, mod);

            if (dep == 0) {
                error_exit(node->loc, "Undeclared identifier `%.*s`.", node->len, node->data);
            }

            bool has_it_already = false;
            for (int i = 0; i < u->dependencies.count; ++i) {
                if (u->dependencies.data[i] == dep) {
                    has_it_already = true;
                    break;
                }
            }

            // TODO: Should we allow self-references in this step?
            // When should they be catched?
            // if (dep != id)
            if (!has_it_already) *append(&u->dependencies) = dep;
        } break;

        case NODE_MUL:
        case NODE_DIV:
        case NODE_MOD:
        case NODE_ADD:
        case NODE_SUB:
        case NODE_EQUAL:
        case NODE_NOT_EQUAL: {
            Node *lhs = node->children.data[0];
            Node *rhs = node->children.data[1];
            process_unit(id, lhs, mod, node);
            process_unit(id, rhs, mod, node);
        } break;

        case NODE_NUM:
        case NODE_STR:
        case NODE_CHAR:
            break;

        default: {
            error_exit(node->loc, "Unimplemented unit %s.", node_type(node->type));
        } break;
    }
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

    // Append nil unit.
    // This will help when we want to always return a valid unit 
    append_zero(&mod->units);

    for (int i = 0; i < count; ++i) {
        Node *node = units[i];
        Unit *unit = append_zero(&mod->units);
        assert(node->children.count >= 1);

        // TODO: Fetch all user-defined types
        switch (node->type) {
            case NODE_CONST: {
                Node *name_node = node->children.data[0];

                unit->type = UNIT_CONSTANT;
                unit->node = node;
                unit->name = name_node->data;
                unit->len = name_node->len;
            } break;
            case NODE_VAR: {
                Node *name_node = node->children.data[0];

                unit->type = UNIT_VARIABLE;
                unit->node = node;
                unit->name = name_node->data;
                unit->len = name_node->len;
            } break;
            case NODE_PROC: {
                Node *call = node->children.data[0];
                assert(call->children.count >= 1);

                unit->type = UNIT_PROC;
                unit->node = node;
                unit->name = call->children.data[0]->data;
                unit->len = call->children.data[0]->len;
            } break;
            default: {
                error_exit(node->loc, "This statement cannot be used as a top-level statement.");
            } break;
        }

        // Check for redefinitions.
        for (int i = 1; i < mod->units.count - 1; ++i) {
            Unit *u = &mod->units.data[i];
            if (u->len != unit->len) continue;
            if (strncmp(u->name, unit->name, unit->len) != 0) continue;
            error(unit->node->loc, "Redefinition of `%.*s`.", unit->len, unit->name);
            note(u->node->loc, "Previous definition is here.");

            if (u->type == UNIT_PROC && unit->type == UNIT_PROC) {
                note(unit->node->loc, "In this language we don't forward-declare anything.");
            }

            compiler_fatal();
        }
    }

    for (int i = 1; i < mod->units.count; ++i) {
        process_unit(i, NULL, mod, NULL);
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
    // print_node(node, 0);

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
    // Units

    printf("digraph {\n");
    for (int i = 1; i < mod.units.count; ++i) {
        Unit *u = &mod.units.data[i];
        if (u->dependencies.count < 1) continue;

        printf("%.*s -> {", u->len, u->name);
        for (int j = 0; j < u->dependencies.count; ++j) {
            if (j > 0) printf(" ");
            Unit *d = unit_ref(u->dependencies.data[j], &mod);
            printf("%.*s", d->len, d->name);
        }
        printf("}\n");
    }
    printf("}\n");

    #endif
    
    return 0;
}
