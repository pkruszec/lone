#include "parser.h"

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
        case NODE_EQUAL: return "equal";
        case NODE_NOT_EQUAL: return "not_equal";
        case NODE_EVAL: return "eval";
        case NODE_CONST: return "const";
        case NODE_ASSIGN: return "assign";
        case NODE_NOP: return "nop";
        case NODE_UNSET: return "unset";
        case NODE_VAR: return "var";
        case NODE_PROC: return "proc";
        case NODE_IF: return "if";
        case NODE_PROC_CALL: return "proc_call";
        case NODE_PROC_ARG: return "proc_arg";
        case NODE_PROC_RETVAL: return "proc_retval";
        case NODE_PROC_BODY: return "proc_body";
        case NODE_IF_COND: return "if_cond";
        case NODE_IF_IF: return "if_if";
        case NODE_IF_ELSE: return "if_else";
        case NODE_BLOCK: return "block";
        case NODE_RETURN: return "return";
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

Node *parser_alloc_node(Parser *parser, Location loc)
{
    Node_Block *last = parser->node_pool.last;
    if (!last || last->count + 1 >= (int)ARRAY_COUNT(last->data)) {
        last = node_pool_add_block(&parser->node_pool);
    }
    Node *node = &last->data[last->count++];
    memset(node, 0, sizeof(*node));
    node->loc = loc;
    return node;
}

Node *node_wrap(Parser *parser, Node *node, Node_Type type)
{
    Node *wrap = parser_alloc_node(parser, node->loc);
    wrap->type = type;
    node_append_child(wrap, node);
    return wrap;
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

void parser_expect_fail(Token *tok, const char *expected)
{
    char buf[64] = {0};
    token_repr(buf, ARRAY_COUNT(buf) - 1, tok);
    error_exit(tok->loc, "Expected %s, got %s.\n", expected, buf);
}

Node *parser_parse_block(Parser *parser)
{
    Token *open_brace = parser_next(parser);
    parser_expect_token_type(open_brace, TOKEN_COPEN);

    Node *node = parser_alloc_node(parser, open_brace->loc);
    node->type = NODE_BLOCK;
    
    while (1) {
        Token *tok = parser_peek(parser);
        if (tok->type == TOKEN_EOF) break;
        if (tok->type == TOKEN_CCLOSE) {
            parser_next(parser);
            return node;
        }

        Node *stmt = parser_parse_stmt(parser);
        node_append_child(node, stmt);
    }

    error_exit(open_brace->loc, "Unmatched '{' in block.");
    return NULL;
}

Node *parser_parse_if(Parser *parser)
{
    Token *iff = parser_next(parser);
    parser_expect_token_type(iff, TOKEN_IF);

    Node *node = parser_alloc_node(parser, iff->loc);
    node->type = NODE_IF;

    Node *condition = parser_parse_expr(parser);
    Node *stmt_if = parser_parse_stmt(parser);
    Node *stmt_else = NULL;

    Token *elze = parser_peek(parser);
    if (elze->type == TOKEN_ELSE) {
        parser_next(parser);
        stmt_else = parser_parse_stmt(parser);
    }

    node_append_child(node, node_wrap(parser, condition, NODE_IF_COND));
    node_append_child(node, node_wrap(parser, stmt_if, NODE_IF_IF));
    if (stmt_else) node_append_child(node, node_wrap(parser, stmt_else, NODE_IF_ELSE));

    return node;
}

Node *parser_parse_stmt(Parser *parser)
{
    Token *peek = parser_peek(parser);
    if (peek->type == TOKEN_EOF) {
        // TODO: Add Hello, World here
        error_exit(peek->loc,
                   "Expected statement, got EOF. Maybe add a main function.\n"
                   "+++ proc main() {\n"
                   "+++ }"
                   );
    } else if (peek->type == TOKEN_SEMICOLON) {
        Node *node = parser_alloc_node(parser, peek->loc);
        node->type = NODE_NOP;
        return node;
    } else if (peek->type == TOKEN_IF) {
        return parser_parse_if(parser);
    } else if (peek->type == TOKEN_COPEN) {
        return parser_parse_block(parser);
    } else if (peek->type == TOKEN_RETURN) {
        parser_next(parser);
        Token *peek2 = parser_peek(parser);
        if (peek2->type == TOKEN_SEMICOLON) {
            parser_next(parser);
            Node *node = parser_alloc_node(parser, peek->loc);
            node->type = NODE_RETURN;
            return node;
        } else {
            Node *node = node_wrap(parser, parser_parse_expr(parser), NODE_RETURN);
            parser_expect_token_type(parser_next(parser), TOKEN_SEMICOLON);
            return node;
        }
    } else if (peek->type == TOKEN_PROC) {
        parser_next(parser);
        Node *call = parser_parse_expr(parser);

        if (call->type != NODE_PROC_CALL) {
            error_exit(call->loc, "Expected a function name, then a list of arguments, for example: `proc add(a: int, b: int) -> int`.");
        }

        Node *proc = parser_alloc_node(parser, peek->loc);
        proc->type = NODE_PROC;
        node_append_child(proc, call);

        peek = parser_peek(parser);
        if (peek->type == TOKEN_ARROW) {
            parser_next(parser);
            Node *retval = node_wrap(parser, parser_parse_expr(parser), NODE_PROC_RETVAL);
            node_append_child(proc, retval);
        }

        // NOTE: Parse all attributes, tags, etc. here

        peek = parser_peek(parser);
        if (peek->type == TOKEN_SEMICOLON) {
            parser_next(parser);
            return proc;
        } else if (peek->type == TOKEN_COPEN) {
            Node *body = node_wrap(parser, parser_parse_block(parser), NODE_PROC_BODY);
            node_append_child(proc, body);
            return proc;
        } else {
            parser_expect_fail(peek, "';' or a block");
        }
        
        return NULL;
    } else if (peek->type == TOKEN_VAR) {
        Node *node = NULL;

        parser_next(parser);
        Node *name = parser_parse_expr(parser);
        if (name->type != NODE_IDENT) {
            error_exit(name->loc, "Cannot name variable with an expression. The syntax for `var` is `var name: type = value;`");
        }

        Node *src = NULL;
        Node *type = NULL;

        Token *tok = parser_next(parser);
        parser_expect_token_type(tok, TOKEN_COLON);

        tok = parser_peek(parser);
        if (tok->type == TOKEN_ASSIGN) {
            parser_next(parser);
            src = parser_parse_expr(parser);
            type = parser_alloc_node(parser, tok->loc);
            type->type = NODE_UNSET;
        } else {
            type = parser_parse_expr(parser);
            tok = parser_peek(parser);
            if (tok->type == TOKEN_ASSIGN) {
                parser_next(parser);
                src = parser_parse_expr(parser);
            } else if (tok->type == TOKEN_SEMICOLON) {
                src = parser_alloc_node(parser, tok->loc);
                src->type = NODE_UNSET;
            } else {
                parser_expect_token_type_msg(tok, TOKEN_SEMICOLON, "'=' or ';'");
            }
        }

        tok = parser_next(parser);
        parser_expect_token_type(tok, TOKEN_SEMICOLON);

        node = parser_alloc_node(parser, tok->loc);
        node->type = NODE_VAR;
        node_append_child(node, name);
        node_append_child(node, src);
        node_append_child(node, type);

        return node;
    } else {
        Node *node = NULL;

        Node *dst = parser_parse_expr(parser);
        Token *tok = parser_next(parser);
        if (tok->type == TOKEN_SEMICOLON) {
            node = parser_alloc_node(parser, dst->loc);
            node->type = NODE_EVAL;
            node_append_child(node, dst);
            return node;
        } else if (tok->type == TOKEN_ASSIGN) {
            Node *src = parser_parse_expr(parser);
            node = parser_alloc_node(parser, tok->loc);
            node->type = NODE_ASSIGN;
            node_append_child(node, dst);
            node_append_child(node, src);
        } else if (tok->type == TOKEN_COLON) {
            if (dst->type != NODE_IDENT) {
                error_exit(dst->loc, "Cannot name constant with an expression. The syntax for constants is `name: type = value;`");
            }

            tok = parser_peek(parser);
            
            if (tok->type == TOKEN_ASSIGN) {
                parser_next(parser);
                Node *src = parser_parse_expr(parser);
                node = parser_alloc_node(parser, dst->loc);
                node->type = NODE_CONST;
                node_append_child(node, dst);
                node_append_child(node, src);
            } else {
                Node *type = parser_parse_expr(parser);
                tok = parser_next(parser);
                if (tok->type != TOKEN_ASSIGN) {
                    error_exit(tok->loc, "Constants must be assigned a value. Maybe add a value or use `var`.");
                }
                Node *src = parser_parse_expr(parser);
                node = parser_alloc_node(parser, dst->loc);
                node->type = NODE_CONST;
                node_append_child(node, dst);
                node_append_child(node, src);
                node_append_child(node, type);
            }

        } else {
            parser_expect_fail(tok, "';', ':', or '='");
        }

        tok = parser_next(parser);
        parser_expect_token_type(tok, TOKEN_SEMICOLON);

        return node;
    }

    return NULL;
}

Node *parser_parse_expr(Parser *parser)
{
    return parser_parse_eq(parser);
}

Node *parser_parse_eq(Parser *parser)
{
    Node *base = parser_parse_add(parser);

    while (1) {
        Token *tok = parser_peek(parser);
        Node *node = NULL;
        if (tok->type == TOKEN_EQUAL) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_EQUAL;
        } else if (tok->type == TOKEN_NOT_EQUAL) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_NOT_EQUAL;
        }
        if (!node) break;

        node_append_child(node, base);
        node_append_child(node, parser_parse_add(parser));
        base = node;
    }
    
    return base;
}

Node *parser_parse_add(Parser *parser)
{
    Node *base = parser_parse_mul(parser);

    while (1) {
        Token *tok = parser_peek(parser);
        Node *node = NULL;
        if (tok->type == TOKEN_PLUS) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_ADD;
        } else if (tok->type == TOKEN_MINUS) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_SUB;
        }
        if (!node) break;

        node_append_child(node, base);
        node_append_child(node, parser_parse_mul(parser));
        base = node;
    }
    
    return base;
}

Node *parser_parse_mul(Parser *parser)
{
    Node *base = parser_parse_unary(parser);

    while (1) {
        Token *tok = parser_peek(parser);
        Node *node = NULL;
        if (tok->type == TOKEN_ASTERISK) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_MUL;
        } else if (tok->type == TOKEN_SLASH) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_DIV;
        } else if (tok->type == TOKEN_MODULO) {
            parser_next(parser);
            node = parser_alloc_node(parser, base->loc);
            node->type = NODE_MOD;
        }
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
            node = parser_alloc_node(parser, tok->loc);
            node->type = NODE_U_PLUS;
        } else if (tok->type == TOKEN_MINUS) {
            parser_next(parser);
            node = parser_alloc_node(parser, tok->loc);
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

    Node *rest = parser_parse_proc_call(parser);
    
    if (first && last) {
        node_append_child(last, rest);
        return first;
    }

    return rest;
}

Node *parser_parse_proc_call(Parser *parser)
{
    Node *proc = parser_parse_term(parser);

    Token *tok = parser_peek(parser);
    if (tok->type == TOKEN_POPEN) {
        parser_next(parser);
        Node *call = parser_alloc_node(parser, tok->loc);
        call->type = NODE_PROC_CALL;
        node_append_child(call, proc);

        if (parser_peek(parser)->type == TOKEN_PCLOSE) {
            parser_next(parser);
            return call;
        }
        
        while (1) {
            Node *fst = parser_parse_expr(parser);
            Node *snd = NULL;
            tok = parser_peek(parser);
            Token *arg_tok = tok;

            if (tok->type == TOKEN_COLON) {
                parser_next(parser);
                snd = parser_parse_expr(parser);
            }

            tok = parser_next(parser);

            Node *arg = parser_alloc_node(parser, arg_tok->loc);
            arg->type = NODE_PROC_ARG;
            node_append_child(arg, fst);
            if (snd) node_append_child(arg, snd);

            node_append_child(call, arg);
            
            if (tok->type == TOKEN_PCLOSE) {
                break;
            } else if (tok->type == TOKEN_COMMA) {
                continue;
            }

            parser_expect_fail(tok, "')' or ','");
        }
        
        return call;
    } else {
        return proc;
    }
}

Node *parser_parse_term(Parser *parser)
{
    Token *tok = parser_next(parser);

    // TODO: Make a lookup table for that
    if (tok->type == TOKEN_NUM) {
        Node *node = parser_alloc_node(parser, tok->loc);
        node->type = NODE_NUM;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    } else if (tok->type == TOKEN_CHAR) {
        Node *node = parser_alloc_node(parser, tok->loc);
        node->type = NODE_CHAR;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    } else if (tok->type == TOKEN_STR) {
        Node *node = parser_alloc_node(parser, tok->loc);
        node->type = NODE_STR;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    } else if (tok->type == TOKEN_SYM) {
        Node *node = parser_alloc_node(parser, tok->loc);
        node->type = NODE_IDENT;
        node->len = tok->len;
        node->data = tok->data;
        return node;
    }

    parser_expect_token_type(tok, TOKEN_POPEN);
    Node *expr = parser_parse_expr(parser);
    tok = parser_next(parser);
    parser_expect_token_type(tok, TOKEN_PCLOSE);
    return expr;
}

