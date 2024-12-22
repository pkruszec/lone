#ifndef PARSER_H
#define PARSER_H

#include "common.h"
#include "lexer.h"

typedef enum {
    // Terminals
    NODE_IDENT,
    NODE_NUM,
    NODE_CHAR,
    NODE_STR,
    // Unary operators
    NODE_U_PLUS,
    NODE_U_MINUS,
    NODE_PROC_CALL,
    // Binary operators
    NODE_MUL,
    NODE_DIV,
    NODE_MOD,
    NODE_ADD,
    NODE_SUB,
    // Statements
    NODE_NOP,
    NODE_EVAL,
    NODE_CONST,
    NODE_VAR,
    NODE_ASSIGN,
    NODE_PROC,
    NODE_BLOCK,
    NODE_RETURN,
    // Meta-types
    NODE_PROC_ARG,
    NODE_PROC_RETVAL,
    NODE_PROC_BODY,
    NODE_UNSET,
    
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
} Parser;

const char *node_type(Node_Type type);

Node *parser_parse_stmt(Parser *parser);
Node *parser_parse_expr(Parser *parser);
Node *parser_parse_add(Parser *parser);
Node *parser_parse_mul(Parser *parser);
Node *parser_parse_unary(Parser *parser);
Node *parser_parse_proc_call(Parser *parser);
Node *parser_parse_term(Parser *parser);

#endif // PARSER_H
