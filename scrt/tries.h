#ifndef tries
#define tries
// Data structure for the nodes of the path trie.

typedef struct int_vector int_vector;
typedef struct char_vector char_vector;
typedef struct node node;

#include "vectors.h"

struct node {
    int node_ind;
    char_vector* valuation;
    struct node* lchild;
    struct node* rchild;
    struct node* parent;
};

// Data structures for representing the NFST.
typedef struct rangeset {
    unsigned char start;
    unsigned char end;
} rangeset;

// Enumeration for the different kinds of states.
enum SType { CHOICE, ACCEPT, SKIP, SYMBOL };

// Choice state.
typedef struct choice_s {
    int t1;
    int t2;
} choice_s;

// Skip state.
// Assumes that output is not '\0'.
typedef struct skip_s {
    int target;
    char* output;
    int olen;
} skip_s;

// Symbol state.
// Assumes that input is not '\0'.
typedef struct symbol_s {
    int target;
    int ilen;
    char* output;
    int olen;
    rangeset* input;
} symbol_s;

// Union state tagged with enum, accept state needs no data so it is not
// represented in the union
typedef struct state {
    enum SType s_type;
    union {
        choice_s choice;
        skip_s   skip;
        symbol_s symbol;
    };
} state;
#endif
