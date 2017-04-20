#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tries.h"

node_vector* pool;

state* parse(char* fp, int* l, int* ss) {
    FILE* fd = fopen(fp, "r");
    int len, starts;
    if (fscanf(fd, "%i %i\n", &len, &starts) != 2) exit(3);
    state* nfst = (state*) malloc(len * sizeof(state));
    *l = len;
    *ss = starts;

    // Parse states
    int ind, t1, t2, out;
    char tmp, tmp2;
    for (int i = 0; i < len; ++i) {
        if (fscanf(fd, "%i %c", &ind, &tmp) != 2) exit(3); // Parse state index and type
        if (ferror(fd)) exit(2);
        switch (tmp) {
            // Choice state
            case 'C':   if (fscanf(fd, "%i %i\n", &t1, &t2) != 2) exit(3); // Parse the two targets
                        nfst[ind] .s_type = CHOICE;
                        nfst[ind] .choice.t1 = t1;
                        nfst[ind] .choice.t2 = t2;
                        break;
            // Skip state
            case 'S':   if (fscanf(fd, "%i %c", &t1, &tmp2) != 2) exit(3); // Parse target and sub type
                        nfst[ind] .s_type = SKIP;
                        nfst[ind] .skip.target = t1;
                        switch (tmp2) {
                            // Writing skip state
                            case 'W':   if (fscanf(fd, " %i [", &t2) != 1) exit(3);
                                        char_vector* output = cvector_create();
                                        char tmp3;
                                        for (int i = 0; i < t2-1; ++i) {
                                            if (fscanf(fd, "%i,", &tmp3) != 1) exit(3);
                                            cvector_append(output, tmp3);
                                        }
                                        if (fscanf(fd, "%i]\n", &tmp3) != 1) exit(3);
                                        cvector_append(output, tmp3);
                                        nfst[ind] .skip.output = output;
                                        break;
                            // No output state
                            case 'E':   if (fscanf(fd, "\n", &tmp2) != 0) exit(3);
                                        nfst[ind] .skip.output = '\0';
                                        break;
                            default: exit(2); // Not supported yet (Register actions)
                        }
                        break;
            // Read (Symbol) state
            // Parse target and number of rangesets.
            case 'R':   if (fscanf(fd, "%i %i [", &t1, &t2) != 2) exit(3);
                        rangeset* rs = (rangeset*) malloc(t2 * sizeof(rangeset));
                        int a, b;

                        // Parse rangesets
                        for (int j = 0; j < t2-1; ++j) {
                            if (fscanf(fd, "(%i,%i),", &a, &b) != 2) exit(3);
                            rs[j] .start = (char) a;
                            rs[j] .end   = (char) b;
                        }
                        if (fscanf(fd, "(%i,%i)] ", &a, &b) != 2) exit(3);
                        rs[t2-1] .start = (char) a;
                        rs[t2-1] .end   = (char) b;

                        if (fscanf(fd, "%c\n", &tmp2) != 1) exit(3); // Parse sub type
                        nfst[ind] .s_type = SYMBOL;
                        nfst[ind] .symbol.target = t1;
                        nfst[ind] .symbol.ilen = t2;
                        nfst[ind] .symbol.input = rs;
                        nfst[ind] .symbol.output = (tmp2 == 'C'); // No output unless its CopyArg (no register actions)
                        break;
            // Accept state
            case 'A':   nfst[ind] .s_type = ACCEPT;
                        break;
        }

    }
    return nfst;
}


// Delete the given node, and all other deterministic ancestors.
void ndelete(node* n, node** root) {
    node* p = n->parent;

    // Deleting the root causes rejection.
    if (p == NULL) {
        printf("Reject\n");
        exit(0);
    }
    n->lchild = NULL;
    n->rchild = NULL;
    n->valuation->len = 0;
    nvector_push(pool, n);
    if (p->rchild == n) {
        node* lchild = p->lchild;
        // Empty node after delete?
        if (lchild == NULL) {
            ndelete(p, root);
        } else {
            // Send data down the tree as to not change leaf pointers.
            cvector_prepend(lchild->valuation, p->valuation);

            node* pp = p->parent;
            if (pp == NULL) {
                // Update root
                *root = lchild;
                lchild->parent = NULL;
            } else if (pp->lchild == p) {
                pp->lchild = lchild;
                lchild->parent = pp;
            } else {
                pp->rchild = lchild;
                lchild->parent = pp;
            }
            free(p);
        }
    } else {
        node* rchild = p->rchild;

        if (rchild == NULL) {
            ndelete(p, root);
        } else {
            cvector_prepend(rchild->valuation, p->valuation);

            node* pp = p->parent;
            if (pp == NULL) {
                *root = rchild;
                rchild->parent = NULL;
            } else if (pp->lchild == p) {
                pp->lchild = rchild;
                rchild->parent = pp;
            } else {
                pp->rchild = rchild;
                rchild->parent = pp;
            }
            free(p);
        }
    }
}

int follow_ep(state* nfst, node* n, node_vector* leafs, int j) {
    state st = nfst[n->node_ind];
    switch (st.s_type) {
        case CHOICE: {
            if (pool->len > 0) {
                n->lchild = nvector_pop(pool);
            } else {
                n->lchild = (node*) malloc(sizeof(node));
                n->lchild->valuation = cvector_create();
            }

            if (pool->len > 0) {
                n->rchild = nvector_pop(pool);
            } else {
                n->rchild = (node*) malloc(sizeof(node));
                n->rchild->valuation = cvector_create();
            }

            //vector_append(n->lchild->valuation, 0);
            n->lchild->parent = n;
            n->lchild->node_ind = st.choice.t1;

            //vector_append(n->rchild->valuation, 1);
            n->rchild->parent = n;
            n->rchild->node_ind = st.choice.t2;

            int t = follow_ep(nfst, n->lchild, leafs, j);
            return follow_ep(nfst, n->rchild, leafs, t+1);
                     }
        case SKIP: {
            if (st.skip.output != '\0') {
                cvector_concat(n->valuation, st.skip.output);
            }
            n->node_ind = st.skip.target;
            return follow_ep(nfst, n, leafs, j);
            break;
                   }
        default: {
            nvector_insert(leafs, n, j);
            return j;
            break;
                 }
    }
}


void output(node_vector* leafs, state* nfst) {
    char_vector* data = cvector_create();
    for (int i = 0; i < leafs->len; ++i) {
        // Is accept state.
        if (nfst[leafs->data[i]->node_ind].s_type == ACCEPT) {
            node *n = leafs->data[i];

            while (n != NULL) {
                cvector_prepend(data, n->valuation);
                n = n->parent;
            }
            break;
        }
    }
    for (int i = 0; i < data->len; ++i) {
        printf("%c", data->data[i]);
    }
    /*state cur_state = nfst[0];
    for (int i = 0; i < data->len; ++i) {
        while (cur_state.s_type != CHOICE) {
            switch (cur_state.s_type) {
                case SYMBOL:
                    if (cur_state.symbol.output != '\0') {
                        printf("%c", cur_state.symbol.output);
                    }
                    cur_state = nfst[cur_state.symbol.target];
                    break;
                case SKIP:
                    printf("%c", cur_state.skip.output);
                    cur_state = nfst[cur_state.skip.target];
                    break;
                default:
                    printf("Accept state??");
                    exit(22); // Probably shouldn't happen.
                    break;
            }
        }
        if (data->data[i] == 0) {
            cur_state = nfst[cur_state.choice.t1];
        } else {
            cur_state = nfst[cur_state.choice.t2];
        }
    }
    */
}


int step(unsigned char input, state* nfst, node* n, node_vector* leafs, int j, node** root) {
    state st = nfst[n->node_ind];
    switch (st.s_type) {
        case SYMBOL: {
            int match = false;
            for (int i = 0; i < st.symbol.ilen; ++i) {
                if (input >= st.symbol.input[i].start && input <= st.symbol.input[i].end) {
                    match = true;
                    break;
                }
            }
            if (match) {
                n->node_ind = st.symbol.target;
                if (st.symbol.output) cvector_append(n->valuation, input);
                return follow_ep(nfst, n, leafs, j);
            }
            } // Fallthrough
        case ACCEPT:
            ndelete(n, root);
            return j-1;
        default:
            exit(23); // Stepping on choice or skip state should not happen.
    }
}


int main(int argc, char** argv) {
    //char* input = "babab";

    if (argc < 2) {
        exit(23);
    }

    pool = nvector_create();

    int nlen, ss;
    state* nfst = parse(argv[1], &nlen, &ss);

    node* root = (node*) malloc(sizeof(node));
    root->valuation = cvector_create();
    root->node_ind = ss;

    node_vector* leafs = nvector_create();
    follow_ep(nfst, root, leafs, 0);

    char input = getchar();
    while (input != '\0' && !feof(stdin)) {
        for (int j = 0; j < leafs->len; ++j) {
            int sti = leafs->data[j]->node_ind;
            state st = nfst[sti];
            node* leaf = leafs->data[j];
            nvector_remove(leafs, j);
            j = step(input, nfst, leaf, leafs, j, &root);
        }
        for (int i = 0; i < root->valuation->len; ++i) {
            putchar(root->valuation->data[i]);
        }
        root->valuation->len = 0;
        input = getchar();
    }
    output(leafs, nfst);
}

