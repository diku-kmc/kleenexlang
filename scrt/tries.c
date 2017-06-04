#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tries.h"

node_vector* pool;
mvector* ns;

state* parse(char* fp, int* l, int* ss, int* ns) {
    FILE* fd = fopen(fp, "r");
    int len, starts;
    if (fscanf(fd, "%i %i\n", &len, &starts) != 2) exit(3);
    state* nfst = (state*) malloc(len * sizeof(state));
    *l = len;
    *ss = starts;
    *ns = 0;

    // Parse states
    int ind, t1, t2;
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
                        // Parse target and sub type
            case 'S':   if (fscanf(fd, "%i %c", &t1, &tmp2) != 2) exit(3);
                        nfst[ind] .s_type = SKIP;
                        nfst[ind] .skip.target = t1;
                        switch (tmp2) {
                            // Writing skip state
                            case 'W':   if (fscanf(fd, " %i\n", &t2) != 1) exit(3);
                                        nfst[ind] .skip.output = (char) t2;
                                        break;
                            // No output state
                            case 'E':   if (fscanf(fd, "\n") != 0) exit(3);
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
                        *ns += 1;
                        break;
            // Accept state
            case 'A':   nfst[ind] .s_type = ACCEPT;
                        *ns += 1;
                        break;
        }

    }
    return nfst;
}

// Simple visualization of the path tree, might not work currently.
void vis_tree(node* n, FILE* fd) {
    if (n->lchild != NULL) {
        n->valuation->data[n->valuation->len] = '\0';
        fprintf(fd, "\"%p\" [shape=circle, label=\"%i\\n%p\\nData:\"];\n",
                n,
                n->node_ind,
                n->valuation);
        fprintf(fd, "\"%p\" -> \"%p\";\n", n, n->lchild);
        //if (n->rchild != NULL)
        fprintf(fd, "\"%p\" -> \"%p\";\n", n, n->rchild);
        vis_tree(n->lchild, fd);
        vis_tree(n->rchild, fd);
    } else {
        n->valuation->data[n->valuation->len] = '\0';
        fprintf(fd, "\"%p\" [shape=circle, label=\"L %i\\n%p\\nData:\"];\n",
                n,
                n->node_ind,
                n->valuation);
    }
}

void vis(node* n, char* fp) {
    FILE* fd = fopen(fp, "w");
    fprintf(fd, "digraph T {\n");
    vis_tree(n, fd);
    fprintf(fd, "}");
    fclose(fd);
}


bool follow_ep(state* nfst, node* n, node_vector* leafs, bool* visited,
        node_vector* del)
{
    int ind = n->node_ind;

    // Stop and mark node for deletion, if the state has been previously
    // visited in current input iteration.
    if (visited[ind]) {
        nvector_push(del, n);
        return false;
    }
    visited[ind] = true;
    state st = nfst[ind];
    switch (st.s_type) {
        case CHOICE: {
            node *lchild, *rchild;

            // Allocate two nodes, from pool or heap.
            if (pool->len > 1) {
                lchild = nvector_pop(pool);
                rchild = nvector_pop(pool);
            } else {
                lchild = (node*) malloc(sizeof(node));
                lchild->valuation = cvector_create();

                rchild = (node*) malloc(sizeof(node));
                rchild->valuation = cvector_create();
            }
            n->lchild = lchild;
            n->rchild = rchild;

            lchild->islchild = true;
            lchild->parent = n;
            lchild->node_ind = st.choice.t1;

            rchild->islchild = false;
            rchild->parent = n;
            rchild->node_ind = st.choice.t2;

            follow_ep(nfst, lchild, leafs, visited, del);
            return follow_ep(nfst, rchild, leafs, visited, del);
                     }
        case SKIP: {
            n->node_ind = st.skip.target;
            if (st.skip.output != '\0') {
                cvector_append(n->valuation, st.skip.output);
            }
            return follow_ep(nfst, n, leafs, visited, del);
            break;
                   }
        default: {
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

static inline void delete(node* n, node** root) {
    node* target;
    node* parent = n->parent;

    if (parent == NULL) {
        printf("Reject!\n");
        exit(111);
    }

    if (n->islchild) {
        target = parent->rchild;
    } else {
        target = parent->lchild;
    }

    n->valuation->len = 0;
    nvector_push(pool, n);

    cvector_prepend(target->valuation, parent->valuation);
    target->parent = parent->parent;
    target->islchild = parent->islchild;
    if (parent->parent == NULL) {
        *root = target;
    } else {
        if (parent->islchild) {
            parent->parent->lchild = target;
        } else {
            parent->parent->rchild = target;
        }
    }
    parent->valuation->len = 0;
    nvector_push(pool, parent);
}


int step(unsigned char input, state* nfst, node* n, node_vector* leafs,
        bool* visited, node_vector* del) {
    state st = nfst[n->node_ind];
    switch (st.s_type) {
        case SYMBOL: {
            for (int i = 0; i < st.symbol.ilen; ++i) {
                if (input >= st.symbol.input[i].start && input <= st.symbol.input[i].end) {
                    n->node_ind = st.symbol.target;
                    if (st.symbol.output) cvector_append(n->valuation, input);
                    return follow_ep(nfst, n, leafs, visited, del);
                }
            }
            } // Fallthrough
        case ACCEPT:
            nvector_push(del, n);
            return 0;
        default:
            exit(23); // Stepping on choice or skip state should not happen.
    }
}

// Mostly unused, just to free as much as possible, to have better use of
// Valgrind for memory checks
void free_tree(node* n) {
    if (n->lchild == NULL) {
        cvector_free(n->valuation);
        free(n);
        return;
    }
    free_tree(n->lchild);
    free_tree(n->rchild);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        // Simple way to check actual size of a node struct.
        printf("Node size: %lu, State size: %lu\n", sizeof(node), sizeof(state));
        exit(23);
    }

    const int interval = 8;

    pool = nvector_create();

    int nlen, ss, num_ras;
    state* nfst = parse(argv[1], &nlen, &ss, &num_ras);
    ns   = mvector_create(num_ras * 2 * interval);

    // ALlocate and initialize root node of the path tree.
    node* root = mvector_get(ns);
    root->valuation = cvector_create();
    root->node_ind = ss;

    bool* visited = (bool*) malloc(sizeof(bool) * nlen);

    // Intitialize differnt needed stuff.
    node_vector* tmp;
    node_vector* leafs = nvector_create();
    node_vector* leafs2 = nvector_create();
    node_vector* del = nvector_create();
    follow_ep(nfst, root, leafs, visited, del);
    memset(visited, 0, sizeof(bool) * nlen);
    char buffer[2048];
    int pos = 0;
    int size = fread(buffer, 1, 2048, stdin);

    char input;
    while (pos < size) {
        input = buffer[pos];
        // Iterate over active leaf nodes.
        for (int j = 0; j < leafs->len; ++j) {
            node* leaf = leafs->data[j];
            step(input, nfst, leaf, leafs2, visited, del);
        }

        // Swap leafs to the newly found ones, and reset the other vector.
        tmp = leafs;
        leafs = leafs2;
        leafs2 = tmp;
        leafs2->len = 0;

        // Prune path tree, and print whatever resides in the root node.
        for (int i = 0; i < del->len; ++i) {
            delete(del->data[i], &root);
        }
        del->len = 0;


        // Reset visited array.
        memset(visited, 0, sizeof(bool) * nlen);

        pos++;
        if (pos >= size) {
            fwrite(root->valuation->data, 1, root->valuation->len, stdout);
            root->valuation->len = 0;
            size = fread(buffer, 1, 2048, stdin);
            pos = 0;
        }
    }
    output(leafs, nfst);
    //fprintf(stderr, "Leafs: %i\n", leafs->len);

    // Free (most) of the used memory, to better use of Valgrind memchecker.
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

