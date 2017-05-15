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
            case 'S':   if (fscanf(fd, "%i %c", &t1, &tmp2) != 2) exit(3); // Parse target and sub type
                        nfst[ind] .s_type = SKIP;
                        nfst[ind] .skip.target = t1;
                        switch (tmp2) {
                            // Writing skip state
                            case 'W':   if (fscanf(fd, " %i [", &t2) != 1) exit(3);
                                        char_vector* output = cvector_create();
                                        char tmp3;
                                        for (int i = 0; i < t2-1; ++i) {
                                            if (fscanf(fd, "%hhi,", &tmp3) != 1) exit(3);
                                            cvector_append(output, tmp3);
                                        }
                                        if (fscanf(fd, "%hhi]\n", &tmp3) != 1) exit(3);
                                        cvector_append(output, tmp3);
                                        nfst[ind] .skip.output = output;
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
                        break;
            // Accept state
            case 'A':   nfst[ind] .s_type = ACCEPT;
                        break;
        }

    }
    return nfst;
}

// Simple visualization of the path tree, might not work currently.
void vis_tree(node* n, FILE* fd) {
    if (!n->leaf) {
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
        n->leaf = true;
        n->del = true;
        nvector_push(del, n);
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
            n->lchild->islchild = true;

            n->lchild->parent = n;
            n->lchild->node_ind = st.choice.t1;

            follow_ep(nfst, n->lchild, leafs, visited, del);

            if (pool->len > 0) {
                n->rchild = nvector_pop(pool);
            } else {
                n->rchild = (node*) malloc(sizeof(node));
                n->rchild->valuation = cvector_create();
            }
            n->rchild->del = false;
            n->rchild->leaf = false;
            n->rchild->islchild = false;


            n->rchild->parent = n;
            n->rchild->node_ind = st.choice.t2;

            return follow_ep(nfst, n->rchild, leafs, visited, del);
                     }
        case SKIP: {
            if (st.skip.output != '\0') {
                cvector_concat(n->valuation, st.skip.output);
            }
            n->node_ind = st.skip.target;
            return follow_ep(nfst, n, leafs, visited, del);
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

void delete(node* n, node** root) {
    node* target;
    node* parent = n->parent;

    if (parent == NULL) {
        printf("Reject!\n");
        exit(111);
    }

    if (n->islchild) {
        parent->lchild = NULL;
        target = parent->rchild;
    } else {
        parent->rchild = NULL;
        target = parent->lchild;
    }

    n->valuation->len = 0;
    nvector_push(pool, n);

    if (target == NULL) {
        delete(parent, root);
    } else {
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
}

// Prunes the path tree, expects to be passed the root node as initial call.
ret prune(node* n) {
    struct ret a;
    a.keep = true;
    if (n->leaf) {
        // Push node to pool, if it is marked for deletion.
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

    if (left.keep) {
        if (right.keep) {
            // Keeping both children, just update pointers to possibly new
            // locations.
            n->lchild = left.target;
            n->rchild = right.target;
            left.target->parent = n;
            right.target->parent = n;
            a.target = n;
        } else {
            // If only one child is to be kept, give that child back, and push
            // current node to pool
            node* target = left.target;
            a.target = target;

            cvector_prepend(target->valuation, n->valuation);
            n->valuation->len = 0;
            nvector_push(pool, n);
        }
    } else {
        if (right.keep) {
            // If only one child is to be kept, give that child back, and push
            // current node to pool
            node* target = right.target;
            a.target = target;

            cvector_prepend(target->valuation, n->valuation);
            n->valuation->len = 0;
            nvector_push(pool, n);
        } else {
            // None of the children are kept, there for nothing to keep.
            a.keep = false;

            n->valuation->len = 0;
            nvector_push(pool, n);
        }
    }
    return a;
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
                    n->leaf = false;
                    return follow_ep(nfst, n, leafs, visited, del); // Use truth value to determine deletion
                }
            }
            } // Fallthrough
        case ACCEPT:
            n->del = true;
            nvector_push(del, n);
            return 0;
        default:
            exit(23); // Stepping on choice or skip state should not happen.
    }
}

// Mostly unused, just to free as much as possible, to have better use of
// Valgrind for memory checks
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
    if (argc < 2) {
        // Simple way to check actual size of a node struct.
        printf("Node size: %lu, State size: %lu\n", sizeof(node), sizeof(state));
        exit(23);
    }

    pool = nvector_create();

    int nlen, ss;
    state* nfst = parse(argv[1], &nlen, &ss);

    // ALlocate and initialize root node of the path tree.
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
    node_vector* del = nvector_create();
    follow_ep(nfst, root, leafs, visited, del);
    memset(visited, 0, sizeof(bool) * nlen);
    char buffer[256];
    int pos = 0;
    int size = fread(buffer, 1, 256, stdin);

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

        fwrite(root->valuation->data, 1, root->valuation->len, stdout);
        root->valuation->len = 0;

        // Reset visited array.
        memset(visited, 0, sizeof(bool) * nlen);

        pos++;
        if (pos >= size) {
            size = fread(buffer, 1, 256, stdin);
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

