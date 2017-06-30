#ifndef tries
#define tries
// Data structure for the nodes of the path trie.

typedef struct int_vector int_vector;
typedef struct char_vector char_vector;
typedef struct node node;

#include "vectors.h"
#include <stdbool.h>

// Struct to return a tuple, used by prune().
typedef struct ret {
    int lstart;
    int lend;
} ret;

enum Child { LEFT, RIGHT };

struct node {
    int node_ind;           // Should probably be called state_ind; index into
                            // the NFST array.

    int c;
    bool islchild;
    bool del;
    char_vector* valuation;
    struct node* lchild;
    struct node* rchild;
    struct node* parent;    // Needed for final output.
};

// Data structures for representing the NFST.
typedef struct rangeset {
    unsigned char start;
    unsigned char end;
} rangeset;


// Enumeration for the different kinds of states.
enum SType { CHOICE, ACCEPT, SKIP, SYMBOL, SET, TEST };

// Choice state.
typedef struct choice_s {
    int t1;
    int t2;
} choice_s;

// Skip state.
// Assumes that output is not '\0'.
typedef struct skip_s {
    int target;
    int len;
    char* output;
} skip_s;

// Symbol state.
// Assumes that input is not '\0'.
typedef struct symbol_s {
    int target;
    int ilen;
    char output;
    rangeset* input;
} symbol_s;

typedef struct set_s {
    int target;
    int k;
} set_s;

typedef struct test_s {
    int target;
} test_s;

// Union state tagged with enum, accept state needs no data so it is not
// represented in the union
typedef struct state {
    enum SType s_type;
    union {
        choice_s choice;
        skip_s   skip;
        symbol_s symbol;
        set_s    set;
        test_s   test;
    };
} state;

typedef struct nfst_s {
    int len;
    int num_ras;
    int start;
    state* states;
} nfst_s;

typedef struct visit {
    ret pos;
    int val;
    node* n;
    bool visited;
} visit;

typedef struct mvector {
    node* data;
    int len;
    int capacity;
} mvector;

mvector* mvector_create(int size) {
    mvector* mvec  = (mvector*) malloc(sizeof(mvector));
    node* narray   = (node*) malloc(size * sizeof(node));

    mvec->data     = narray;
    mvec->len      = 0;
    mvec->capacity = size;

    return mvec;
}

void mvector_free(node_vector* vec) {
    free(vec->data);
    free(vec);
}

void mvector_grow(mvector* vec) {
    vec->capacity += VECTOR_INITIAL_CAPACITY;
    vec->data = realloc(vec->data, sizeof(node) * vec->capacity);
    return;
}

node* mvector_get(mvector* vec) {
    node* ret = &(vec->data[vec->len]);
    ret->valuation = cvector_create();
    vec->len++;

    return ret;
}
#endif
