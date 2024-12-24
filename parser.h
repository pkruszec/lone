#ifndef PARSER_H
#define PARSER_H

#include "common.h"
#include "lexer.h"

#define NODES(x)\
    /* Types */\
    x(T_PTR)\
    /* Terminal*/\
    x(IDENT)\
    x(NUM)\
    x(CHAR)\
    x(STR)\
    /* Unary operators*/\
    x(U_PLUS)\
    x(U_MINUS)\
    x(PROC_CALL)\
    x(DEREF)\
    x(ADDROF)\
    x(MEMBER)\
    x(CAST)\
    /* Binary operators*/\
    x(MUL)\
    x(DIV)\
    x(MOD)\
    x(ADD)\
    x(SUB)\
    x(EQUAL)\
    x(NOT_EQUAL)\
    /* Statements*/\
    x(NOP)\
    x(EVAL)\
    x(CONST)\
    x(VAR)\
    x(ASSIGN)\
    x(PROC)\
    x(BLOCK)\
    x(RETURN)\
    x(IF)\
    /* Program*/\
    x(PROG)\
    /* Meta-types*/\
    x(PROC_ARG)\
    x(PROC_RETVAL)\
    x(PROC_BODY)\
    x(IF_COND)\
    x(IF_IF)\
    x(IF_ELSE)\
    x(UNSET)\
    /* Count*/\
    x(COUNT)

#define NODE_ENUM(e) NODE_##e,
#define NODE_SWITCH(e) case NODE_##e: return #e;

typedef enum {
    NODES(NODE_ENUM)
} Node_Type;

typedef struct Node Node;
typedef struct Node_Block Node_Block;

typedef struct {
    Node **data;
    int count;
    int allocated;
} Node_Children;

struct Node {
    Location loc;
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
    Location loc;
} Parser;

const char *node_type(Node_Type type);
Node *node_child(Node *node, int index);
Node *node_unwrap(Node *node);

Node *parser_parse_prog(Parser *parser);
Node *parser_parse_stmt(Parser *parser);
Node *parser_parse_expr(Parser *parser);
Node *parser_parse_eq(Parser *parser);
Node *parser_parse_add(Parser *parser);
Node *parser_parse_mul(Parser *parser);
Node *parser_parse_unary(Parser *parser);
Node *parser_parse_cast(Parser *parser);
Node *parser_parse_proc_call(Parser *parser);
Node *parser_parse_addr(Parser *parser);
Node *parser_parse_term(Parser *parser);

#endif // PARSER_H
