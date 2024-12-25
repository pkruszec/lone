#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
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
        Node *child = node_child(node, i);
        print_node(child, indent + 2);
    }
}

typedef enum {
    SYM_NONE = 0,
    SYM_PROC,
    SYM_GLOBAL_VAR,
    SYM_GLOBAL_CONST,
} Symbol_Type;

typedef struct {
    String_View name;
    Symbol_Type type;
    Node *node;
} Symbol;

typedef struct {
    Symbol *data;
    int count;
    int allocated;
} Symbol_Table;

void symbols_load_prog(Node *prog, Symbol_Table *st)
{
    for (int i = 0; i < prog->children.count; ++i) {
        Node *child = node_child(prog, i);

        Symbol *sym = append_zero(st);
        sym->node = child;

        switch (child->type) {
            case NODE_CONST: {
                Node *name = node_child(child, 0);
                sym->type = SYM_GLOBAL_CONST;
                sym->name = sv_make(name->data, name->len);
            } break;
            case NODE_PROC: {
                Node *name = node_unwrap(node_child(child, 0));
                sym->type = SYM_PROC;
                sym->name = sv_make(name->data, name->len);
            } break;
            case NODE_VAR: {
                Node *name = node_child(child, 0);
                sym->type = SYM_GLOBAL_VAR;
                sym->name = sv_make(name->data, name->len);
            } break;
            default: {
                error_exit(child->loc, "Invalid top-level statement.");
            } break;
        }
    }
}

typedef enum {
    OP_NONE = 0,
    OP_DEF_CONST,
    OP_DEF_VAR,
    OP_PROC,
    OP_END,
    OP_RET,
    OP_MOV_IMM,
    OP_MOV_REG,
    OP_ADD,
} Op_Type;

typedef struct {
    Op_Type type;
    union {
        struct {
            String_View name;
        } def;
        struct {
            String_View name;
            int argc;
            int min_regs;
            bool ret;
        } proc;
        struct {
            int dst;
            uint64_t imm;
        } imm;
        struct {
            int dst;
            int src;
        } reg;
    };
} Op;

typedef struct {
    Op *data;
    int count;
    int allocated;

    Symbol_Table *st;
} Module;

typedef struct {
    String_View name;
    int reg;
    // bool indirect;
} Binding;

typedef struct {
    Binding *data;
    int count;
    int allocated;

    int reg;
    int scope;
    int max;
    bool ret;
} Bindings;

Binding *binding_get(Bindings *bs, String_View name)
{
    for (int i = 0; i < bs->count; ++i) {
        if (!sv_equal(bs->data[i].name, name)) continue;
        return &bs->data[i];
    }

    return NULL;
}

int push_reg(Bindings *bs)
{
    int reg = bs->reg;
    bs->reg++;
    if (bs->reg > bs->max) bs->max = bs->reg;
    return reg;
}

void compile_global_constant(Symbol *sym, Module *mod)
{
    append_value(mod, ((Op) {
        .type = OP_DEF_CONST,
        .def = { .name = sym->name },
    }));
}

void compile_global_var(Symbol *sym, Module *mod)
{
    append_value(mod, ((Op) {
        .type = OP_DEF_VAR,
        .def = { .name = sym->name },
    }));
}

uint64_t compile_parse_base(Location loc, String_View full, String_View sv, int base)
{
    int max = 20;
    if (sv.count > max) {
        error_exit(loc, "Integer literal too big for u64: `%.*s`.", sv_unwrap(full));
    }

    uint64_t x = 0;
    uint64_t m = 1;
    for (int i = sv.count - 1; i >= 0; --i) {
        char c = sv.data[i];

        if (!(is_digit(c) || (base == 16 && is_hex_digit(c)))) {
            error_exit(loc, "Invalid integer literal of base %d `%.*s`.", base, sv_unwrap(full));
        }

        int digit = base == 16 ? get_hex_digit(c) : (c - '0');
        if (digit >= base) {
            error_exit(loc, "Digit too big for base %d: `%c`.", base, c);
        }

        x += digit * m;
        m *= base;
    }

    return x;
}

uint64_t compile_parse_num(Location loc, String_View sv)
{
    assert(sv.count >= 1);

    if (sv_has(sv, '.') >= 0) {
        error_exit(loc, "Float literals are unimplemented.");
    }

    int base = 10;

    if (sv.data[0] == '0' && sv.count >= 2) {
        if (sv.data[1] == 'x') {
            base = 16;
        } else if (sv.data[1] == 'o') {
            base = 8;
        } else if (sv.data[1] == 'b') {
            base = 2;
        } else {
            error_exit(loc, "Invalid base specifier `%c`. Allowed base specifiers are 16 (0x), 8 (0o), 2 (0b).");
        }

        sv.count -= 2;
        sv.data += 2;

        if (sv.count < 1) {
            error_exit(loc, "Expected a number after its base: `%.*s`.");
        }
    }

    return compile_parse_base(loc, sv, sv, base);
}

void compile_expr(int dst, Node *expr, Module *mod, Bindings *bs)
{
    switch (expr->type) {
        case NODE_NUM: {
            uint64_t imm = compile_parse_num(expr->loc, sv_make(expr->data, expr->len));

            append_value(mod, ((Op) {
                .type = OP_MOV_IMM,
                .imm = {
                    .dst = dst,
                    .imm = imm,
                },
            }));
        } break;
        case NODE_IDENT: {
            Binding *b = binding_get(bs, sv_make(expr->data, expr->len));
            
            if (b) {
                append_value(mod, ((Op) {
                    .type = OP_MOV_REG,
                    .reg = {
                        .dst = dst,
                        .src = b->reg,
                    },
                }));
            } else {
                assert(!"TODO");
            }
        } break;
        case NODE_ADD: {
            Node *lhs = node_child(expr, 0);
            Node *rhs = node_child(expr, 1);

            int r0 = push_reg(bs);
            int r1 = push_reg(bs);
            compile_expr(r0, lhs, mod, bs);
            compile_expr(r1, rhs, mod, bs);

            append_value(mod, ((Op) {
                .type = OP_ADD,
                .reg = {
                    .dst = r0,
                    .src = r1,
                },
            }));
            append_value(mod, ((Op) {
                .type = OP_MOV_REG,
                .reg = {
                    .dst = dst,
                    .src = r0,
                },
            }));
        } break;
        default: {
            error_exit(expr->loc, "Unimplemented expression %s.", node_type(expr->type));
        } break;
    }
}

void compile_assign(Node *dst, Node *src, Module *mod, Bindings *bs)
{
    // TODO: Non-register types
    int reg = push_reg(bs);
    compile_expr(reg, src, mod, bs);

    switch (dst->type) {
        case NODE_IDENT: {
            compile_expr(reg, src, mod, bs);

            Binding *b = binding_get(bs, sv_make(dst->data, dst->len));
            
            if (b) {
                append_value(mod, ((Op) {
                    .type = OP_MOV_REG,
                    .reg = {
                        .dst = b->reg,
                        .src = reg,
                    },
                }));
            } else {
                assert(!"TODO");
            }

        } break;
        default: {
            error_exit(dst->loc, "Unimplemented assignment form %s.", node_type(dst->type));
        }
    }
}

void compile_stmt(Node *stmt, Module *mod, Bindings *bs)
{
    switch (stmt->type) {
        case NODE_BLOCK: {
            int scope = bs->scope;
            int count = bs->count;
            bs->scope = bs->reg;
            for (int i = 0; i < stmt->children.count; ++i) {
                compile_stmt(node_child(stmt, i), mod, bs);
            }
            bs->scope = scope;
            bs->count = count;
        } break;
        case NODE_VAR: {
            Node *name = node_child(stmt, 0);
            Node *value = node_child(stmt, 1);

            int reg = push_reg(bs);
            append_value(bs, ((Binding) {
                .name = sv_make(name->data, name->len),
                .reg = reg,
                // .indirect = false,
            }));

            compile_expr(reg, value, mod, bs);
        } break;
        case NODE_ASSIGN: {
            Node *dst = node_child(stmt, 0);
            Node *src = node_child(stmt, 1);
            compile_assign(dst, src, mod, bs);
        } break;
        case NODE_RETURN: {
            if (stmt->children.count == 0) {
                append_value(mod, ((Op) {.type = OP_RET}));
            } else {
                assert(bs->ret);
                int dst_reg = 0;
                compile_expr(dst_reg, node_unwrap(stmt), mod, bs);
                append_value(mod, ((Op) {.type = OP_RET}));
            }
        } break;
        default: {
            error_exit(stmt->loc, "Unimplemented statement %s.", node_type(stmt->type));
        } break;
    }
}

void compile_proc(Symbol *sym, Module *mod)
{
    Node *proc = sym->node;
    Node *call = node_child(proc, 0);
    Node *ret  = NULL;
    Node *body = NULL;

    if (proc->children.count == 3) {
        ret = node_child(proc, 1);
        body = node_child(proc, 2);
    } else if (proc->children.count == 2) {
        Node *snd = node_child(proc, 1);
        if (snd->type == NODE_PROC_RETVAL)
            ret = snd;
        else
            body = snd;
    }

    assert(body != NULL);
    ret = node_unwrap(ret);
    body = node_unwrap(body);

    int argc = call->children.count - 1;

    int op_proc = append_value(mod, ((Op) {
        .type = OP_PROC,
        .proc = { .name = sym->name, .ret = !!ret, .argc = argc },
    }));

    Bindings bindings = {0};
    if (ret) {
        bindings.ret = true;
    }
    push_reg(&bindings);
    bindings.scope = 1;

    for (int i = 1; i < call->children.count; ++i) {
        Node *cc = node_child(call, i);
        if (cc->children.count < 2) continue;

        Node *arg = node_child(cc, 0);
        assert(arg->type == NODE_IDENT);

        int reg = push_reg(&bindings);
        append_value(&bindings, ((Binding) {
            .name = sv_make(arg->data, arg->len),
            .reg = reg,
            // .indirect = false,
        }));
    }

    compile_stmt(body, mod, &bindings);
    mod->data[op_proc].proc.min_regs = bindings.max + 1;

    // for (int i = 0; i < bindings.count; ++i) {
    //     Binding *b = &bindings.data[i];
    //     printf("binding %.*s -> %d\n", sv_unwrap(b->name), b->reg);
    // }

    append_value(mod, ((Op) {.type = OP_END}));
}

void compile_prog(Module *mod)
{
    Symbol_Table *st = mod->st;
    for (int i = 0; i < st->count; ++i) {
        Symbol *sym = &st->data[i];
        switch (sym->type) {
            case SYM_GLOBAL_CONST: {
                compile_global_constant(sym, mod);
            } break;
            case SYM_GLOBAL_VAR: {
                compile_global_var(sym, mod);
            } break;
            case SYM_PROC: {
                compile_proc(sym, mod);
            } break;
            default: break;
        }
    }
}

void print_mod(Module *mod)
{
    int indent = 0;
    int step = 2;

    for (int i = 0; i < mod->count; ++i) {
        Op *op = &mod->data[i];

        printf("%03X  ", i);

        if (op->type == OP_END) indent -= step;
        for (int j = 0; j < indent; ++j)
            fputc(' ', stdout);
        if (op->type == OP_PROC) indent += step;

        switch (op->type) {
            case OP_NONE: printf("nop"); break;
            case OP_DEF_CONST: printf("defconst %.*s", sv_unwrap(op->def.name)); break;
            case OP_DEF_VAR: printf("defvar %.*s", sv_unwrap(op->def.name)); break;
            case OP_PROC: printf("proc %.*s %d %d %s", sv_unwrap(op->proc.name), op->proc.argc, op->proc.min_regs, op->proc.ret ? "ret" : ""); break;
            case OP_END: printf("end"); break;
            case OP_RET: printf("ret"); break;
            case OP_MOV_IMM: printf("imm R%d <- %llu", op->imm.dst, op->imm.imm); break;
            case OP_MOV_REG: printf("reg R%d <- R%d", op->reg.dst, op->reg.src); break;
            case OP_ADD: printf("add R%d <- R%d", op->reg.dst, op->reg.src); break;
        }

        fputc('\n', stdout);
    }
}

uint64_t exec_proc(Module *mod, uint64_t *args, uint64_t ip, int depth)
{
    uint64_t regs[128] = {0};

    Op *proc = &mod->data[ip];
    // TODO: actual error messages
    assert(proc->type == OP_PROC);
    assert(proc->proc.min_regs <= (int)ARRAY_COUNT(regs));
    assert(proc->proc.min_regs >= 1);
    assert(ip < (uint64_t)mod->count - 1);
    
    int max = 1000;
    if (depth > max) {
        error_exit(((Location){0}), "Max recursion depth exceeded (%d) in procedure `%.*s`.", max, sv_unwrap(proc->proc.name));
    }

    for (int i = 0; i < proc->proc.argc; ++i) {
        regs[i + 1] = args[i];
    }

    for (ip += 1; ip < (uint64_t)mod->count; ++ip) {
        Op *op = &mod->data[ip];

        switch (op->type) {
            case OP_NONE: break;

            case OP_MOV_REG: regs[op->reg.dst] = regs[op->reg.src]; break;
            case OP_MOV_IMM: regs[op->imm.dst] = op->imm.imm; break;
            case OP_ADD: regs[op->reg.dst] += regs[op->reg.src]; break;

            case OP_RET:
            case OP_END: return regs[0];

            default: error_exit(((Location){0}), "Unimplemented operation at %03X.", ip); break;
        }
    }

    return regs[0];
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

    Symbol_Table st = {0};
    symbols_load_prog(node, &st);

    // for (int i = 0; i < st.count; ++i) {
    //     printf("%.*s\n", st.data[i].name.count, st.data[i].name.data);
    // }

    Module mod = {0};
    mod.st = &st;
    compile_prog(&mod);

    print_mod(&mod);

    uint64_t args[1] = {6};
    uint64_t result = exec_proc(&mod, args, 0x000, 0);
    printf("Result: %zu (%zd)\n", result, (ptrdiff_t)result);

    return 0;
}
