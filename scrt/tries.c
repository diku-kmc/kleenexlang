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
                            case 'W':   if (fscanf(fd, " %i\n", &t2) != 1) exit(3);
                                        nfst[ind] .skip.output = (char) t2;
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

void vis_tree(node* n, FILE* fd) {
    if (!n->leaf) {
        n->valuation->data[n->valuation->len] = '\0';
        fprintf(fd, "%i [shape=circle, label=\"%i\\n%i\\nData:\"];\n",
                n,
                n->node_ind,
                n->valuation->len,
                n->valuation->data);
        fprintf(fd, "%i -> %i;\n", n, n->lchild);
        //if (n->rchild != NULL)
        fprintf(fd, "%i -> %i;\n", n, n->rchild);
        vis_tree(n->lchild, fd);
        vis_tree(n->rchild, fd);
    } else {
        n->valuation->data[n->valuation->len] = '\0';
        fprintf(fd, "%i [shape=circle, label=\"L %i\\n%i\\nData:\"];\n",
                n,
                n->node_ind,
                n->valuation->len,
                n->valuation->data);
    }
}

void vis(node* n, char* fp) {
    FILE* fd = fopen(fp, "w");
    fprintf(fd, "digraph T {\n");
    vis_tree(n, fd);
    fprintf(fd, "}");
    fclose(fd);
}


bool follow_ep(state* nfst, node* n, node_vector* leafs, bool* visited) {
    int ind = n->node_ind;
    if (visited[ind]) {
        n->leaf = true;
        n->del = true;
        return false;
    }
    visited[ind] = true;
    state st = nfst[ind];
    switch (st.s_type) {
        case CHOICE: {
            if (pool->len > 0) {
                n->lchild = nvector_pop(pool);
            } else {
                n->lchild = (node*) malloc(sizeof(node));
                n->lchild->valuation = cvector_create();
            }
            n->lchild->del = false;
            n->lchild->leaf = false;

            n->lchild->parent = n;
            n->lchild->node_ind = st.choice.t1;

            follow_ep(nfst, n->lchild, leafs, visited);

            if (pool->len > 0) {
                n->rchild = nvector_pop(pool);
            } else {
                n->rchild = (node*) malloc(sizeof(node));
                n->rchild->valuation = cvector_create();
            }
            n->rchild->del = false;
            n->rchild->leaf = false;


            n->rchild->parent = n;
            n->rchild->node_ind = st.choice.t2;

            return follow_ep(nfst, n->rchild, leafs, visited);
                     }
        case SKIP: {
            if (st.skip.output != '\0') {
                cvector_append(n->valuation, st.skip.output);
            }
            n->node_ind = st.skip.target;
            return follow_ep(nfst, n, leafs, visited);
            break;
                   }
        default: {
            n->leaf = true;
            nvector_append(leafs, n);
            return true;
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
        putchar(data->data[i]);
    }
    //cvector_free(data);
}


ret prune(node* n) {
    struct ret a;
    a.keep = true;
    if (n->leaf) {
        if (n->del) {
            a.keep = false;
            n->del = false;
            n->valuation->len = 0;
            nvector_push(pool, n);
        }

        a.target = n;
        return a;
    }

    struct ret left  = prune(n->lchild);
    struct ret right = prune(n->rchild);

    // Maybe goto/cmov in the future
    if (left.keep) {
        if (right.keep) {
            n->lchild = left.target;
            n->rchild = right.target;
            left.target->parent = n;
            right.target->parent = n;
            a.target = n;
        } else {
            node* target = left.target;
            a.target = target;

            cvector_prepend(target->valuation, n->valuation);
            n->valuation->len = 0;
            nvector_push(pool, n);
        }
    } else {
        if (right.keep) {
            node* target = right.target;
            a.target = target;

            cvector_prepend(target->valuation, n->valuation);
            n->valuation->len = 0;
            nvector_push(pool, n);
        } else {
            a.keep = false;

            n->valuation->len = 0;
            nvector_push(pool, n);
        }
    }
    return a;
}




int step(unsigned char input, state* nfst, node* n, node_vector* leafs, int j, node** root,
        bool* visited) {
    state st = nfst[n->node_ind];
    switch (st.s_type) {
        case SYMBOL: {
            int match = false;
            for (int i = 0; i < st.symbol.ilen; ++i) {
                if (input >= st.symbol.input[i].start && input <= st.symbol.input[i].end) {
                    n->node_ind = st.symbol.target;
                    if (st.symbol.output) cvector_append(n->valuation, input);
                    n->leaf = false;
                    return follow_ep(nfst, n, leafs, visited); // Use truth value to determine deletion
                }
            }
            } // Fallthrough
        case ACCEPT:
            n->del = true;
            return j-1;
        default:
            exit(23); // Stepping on choice or skip state should not happen.
    }
}

void free_tree(node* n) {
    if (n->leaf) {
        cvector_free(n->valuation);
        free(n);
        return;
    }
    free_tree(n->lchild);
    free_tree(n->rchild);
}

int main(int argc, char** argv) {
    //char* input = "babab";

    if (argc < 2) {
        printf("%i\n", sizeof(node));
        exit(23);
    }

    pool = nvector_create();

    int nlen, ss;
    state* nfst = parse(argv[1], &nlen, &ss);

    node* root = (node*) malloc(sizeof(node));
    root->valuation = cvector_create();
    root->node_ind = ss;
    root->del = false;
    root->leaf = false;

    bool* visited = (bool*) malloc(sizeof(bool) * nlen);

    // Intitialize differnt needed stuff.
    node_vector* tmp;
    node_vector* leafs = nvector_create();
    node_vector* leafs2 = nvector_create();
    follow_ep(nfst, root, leafs, visited);
    memset(visited, 0, sizeof(bool) * nlen);

    bool pr = false;

    unsigned int k = 0;
    char input = getchar();
    while (input != '\0' && !feof(stdin)) {
        for (int j = 0; j < leafs->len; ++j) {
            node* leaf = leafs->data[j];
            step(input, nfst, leaf, leafs2, j, &root, visited);
        }

        tmp = leafs;
        leafs = leafs2;
        leafs2 = tmp;
        leafs2->len = 0;

        k++;

        // Prune path tree
        if (k % 8 == 0) {
            struct ret a = prune(root);
            if (!a.keep) {
                printf("Reject!");
                exit(11);
            }
            root = a.target;
            root->parent = NULL;

            root->valuation->data[root->valuation->len] = '\0';
            fputs(root->valuation->data, stdout);
            root->valuation->len = 0;
        }

        memset(visited, 0, sizeof(bool) * nlen);

        input = getchar();
    }
    output(leafs, nfst);
    //fprintf(stderr, "Leafs: %i\n", leafs->len);

    // Free (most) of the used memory, to better use Valgrind.
    /*free(nfst);
    free(visited);
    free_tree(root);
    for (int i = 0; i < pool->len; ++i) {
        cvector_free(pool->data[i]->valuation);
        free(pool->data[i]);
    }
    nvector_free(pool);
    nvector_free(leafs);
    nvector_free(leafs2);
    */
}

