#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "tries.h"

node_vector* pool;
mvector* ns;

void parse_nfst(FILE* fd, nfst_s* nfst) {
    state* states = nfst->states;

    // Parse states
    int ns = 0;
    int ind, t1, t2;
    char tmp, tmp2;
    for (int i = 0; i < nfst->len; ++i) {
        if (fscanf(fd, "%i %c", &ind, &tmp) != 2) exit(3); // Parse state index and type
        if (ferror(fd)) exit(2);
        switch (tmp) {
            // Choice state
            case 'C':   if (fscanf(fd, "%i %i\n", &t1, &t2) != 2) exit(3); // Parse the two targets
                        states[ind] .s_type = CHOICE;
                        states[ind] .choice.t1 = t1;
                        states[ind] .choice.t2 = t2;
                        break;
            // Skip state
                        // Parse target and sub type
            case 'S':   if (fscanf(fd, "%i %c", &t1, &tmp2) != 2) exit(3);
                        states[ind] .s_type = SKIP;
                        states[ind] .skip.target = t1;
                        switch (tmp2) {
                            // Writing skip state
                            case 'W':   if (fscanf(fd, " %i [", &t2) != 1) exit(3);
                                        char* output = malloc(t2 * sizeof(char));
                                        char tmp3;
                                        for (int i = 0; i < t2-1; ++i) {
                                            if (fscanf(fd, "%hhi,", &tmp3) != 1) exit(3);
                                            output[i] = tmp3;
                                        }
                                        if (fscanf(fd, "%hhi]\n", &tmp3) != 1) exit(3);
                                        output[t2-1] = tmp3;
                                        states[ind] .skip.output = output;
                                        states[ind] .skip.len = t2;
                                        break;
                            // No output state
                            case 'E':   if (fscanf(fd, "\n") != 0) exit(3);
                                        exit(4);
                                        states[ind] .skip.output = NULL;
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
                        states[ind] .s_type = SYMBOL;
                        states[ind] .symbol.target = t1;
                        states[ind] .symbol.ilen = t2;
                        states[ind] .symbol.input = rs;
                        states[ind] .symbol.output = (tmp2 == 'C'); // No output unless its CopyArg (no register actions)
                        ns += 1;
                        break;
            // Accept state
            case 'A':   states[ind] .s_type = ACCEPT;
                        ns += 1;
                        break;
            case 'I':   states[ind] .s_type = SET;
                        if (fscanf(fd, "%i %i\n", &t1, &t2) != 2) exit(3);
                        states[ind].set.target = t1;
                        states[ind].set.k = t2;
                        break;
            case 'T':   states[ind].s_type = TEST;
                        if (fscanf(fd, "%i\n", &t1) != 1) exit(3);
                        states[ind].test.target = t1;
                        break;
            default: printf("Parse error, unkown state type");
                     exit(11);
        }

    }
    nfst->num_ras = ns;
}

nfst_s* parse(char* fp, int* len) {
    FILE* fd = fopen(fp, "r");
    int slen, starts, num_nfsts;
    if (fscanf(fd, "%i\n", &num_nfsts) != 1) exit(3);
    nfst_s* nfsts = (nfst_s*) malloc(num_nfsts * sizeof(nfst_s));
    for (int i = 0; i < num_nfsts; ++i) {
        if (fscanf(fd, "%i %i ", &slen, &starts) != 2) exit(3);
        nfsts[i].start  = starts;
        nfsts[i].states = (state*) malloc(slen * sizeof(state));
        nfsts[i].len    = slen;
    }

    for (int i = 0; i < num_nfsts; ++i) {
        parse_nfst(fd, nfsts + i);
    }

    *len = num_nfsts;

    return nfsts;
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

void print_leafs(node_vector* leafs, char* fp) {
    FILE* fd = fopen(fp, "a");
    for (int i = 0; i < leafs->len; ++i) {
        fprintf(fd, "%i ", leafs->data[i]->node_ind);
    }
    fprintf(fd, "\n");
    fclose(fd);
}

void purge(node* n, node_vector* leafs, ret r, node_vector* del) {
    for (int i = r.lstart; i <= r.lend; ++i) {
        nvector_push(del, leafs->data[i]);
    }
    nvector_purge(leafs, r.lstart, r.lend + 1);
}

ret follow_ep(state* nfst, node* n, node_vector* leafs, visit* visited,
        node_vector* del)
{
    int ind = n->node_ind;

    ret r = {-1, -1};
    // Stop and mark node for deletion, if the state has been previously
    // visited in current input iteration.
    if (visited[ind].visited) {
        if (visited[ind].val >= n->c) {
            nvector_push(del, n);
            return r;
        } else {
            purge(visited[ind].n, leafs, visited[ind].pos, del);
        }
    }
    visited[ind].visited = true;
    visited[ind].n = n;
    visited[ind].val = n->c;
    state st = nfst[ind];
    switch (st.s_type) {
        case CHOICE: {
            node *lchild, *rchild;

            // Allocate two nodes, from pool or heap.
            if (pool->len > 1) {
                lchild = nvector_pop(pool);
                rchild = nvector_pop(pool);
            } else {
                //lchild = mvector_get(ns);
                lchild = (node*) malloc(sizeof(node));
                lchild->valuation = cvector_create();

                //rchild = mvector_get(ns);
                rchild = (node*) malloc(sizeof(node));
                rchild->valuation = cvector_create();
            }
            lchild->c = n->c;
            rchild->c = n->c;

            n->lchild = lchild;
            n->rchild = rchild;

            lchild->islchild = true;
            lchild->parent = n;
            lchild->node_ind = st.choice.t1;

            rchild->islchild = false;
            rchild->parent = n;
            rchild->node_ind = st.choice.t2;

            ret lc = follow_ep(nfst, lchild, leafs, visited, del);
            ret rc = follow_ep(nfst, rchild, leafs, visited, del);
            r.lstart = (lc.lstart == -1) ? rc.lstart : lc.lstart;
            r.lend   = (rc.lend == -1) ? lc.lend : rc.lend;
            return r;
                     }
        case SKIP: {
            n->node_ind = st.skip.target;
            cvector_concat(n->valuation, st.skip.output, st.skip.len);
            ret c = follow_ep(nfst, n, leafs, visited, del);
            visited[ind].pos = c;
            return c;
                   }
        case SET: {
            n->c = st.set.k;
            n->node_ind = st.set.target;
            ret c = follow_ep(nfst, n, leafs, visited, del);
            visited[ind].pos = c;
            return c;
                  }
        case TEST: {
            if (n->c > 0) {
                n->c--;
                n->node_ind = st.test.target;
                ret c = follow_ep(nfst, n, leafs, visited, del);
                visited[ind].pos = c;
                return c;
            } else {
                nvector_append(del, n);
                return r;
            }
                   }
        default: {
            n->lchild = NULL;
            n->rchild = NULL;
            r.lstart = nvector_append(leafs, n);
            r.lend = r.lstart;
            visited[ind].pos = r;
            return r;
            break;
                 }
    }
}


void foutput(node_vector* leafs, state* nfst, FILE* output) {
    char_vector* data = cvector_create();
    for (int i = 0; i < leafs->len; ++i) {
        // Is accept state.
        if (leafs->data[i] == NULL) {
            continue;
        }
        if (nfst[leafs->data[i]->node_ind].s_type == ACCEPT) {
            node *n = leafs->data[i];

            while (n != NULL) {
                cvector_prepend(data, n->valuation);
                n = n->parent;
            }
            goto end;
        }
    }
    printf("Reject!");
    exit(0);

end:
    fwrite(data->data, 1, data->len, output);
    //cvector_free(data);
}

inline static void delete(node* n, node** root) {
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
    n->lchild = NULL;
    n->rchild = NULL;
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
    parent->lchild = NULL;
    parent->rchild = NULL;
    nvector_push(pool, parent);
}


int step(unsigned char input, state* nfst, node* n, node_vector* leafs,
        visit* visited, node_vector* del) {
    state st = nfst[n->node_ind];
    switch (st.s_type) {
        case SYMBOL: {
            for (int i = 0; i < st.symbol.ilen; ++i) {
                if (input >= st.symbol.input[i].start && input <= st.symbol.input[i].end) {
                    n->node_ind = st.symbol.target;
                    if (st.symbol.output) cvector_append(n->valuation, input);
                    follow_ep(nfst, n, leafs, visited, del);
                    return 0;
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

void simulate(nfst_s nfst, FILE* input, FILE* output) {
    ns   = mvector_create(nfst.num_ras * 4);

    // ALlocate and initialize root node of the path tree.
    node* root = mvector_get(ns);
    root->valuation = cvector_create();
    root->node_ind = nfst.start;
    root->c = 0;

    visit* visited = (visit*) malloc(sizeof(visit) * nfst.len);
    memset(visited, 0, sizeof(visit) * nfst.len);

    // Intitialize differnt needed stuff.
    node_vector* tmp;
    node_vector* leafs = nvector_create();
    node_vector* leafs2 = nvector_create();
    node_vector* del = nvector_create();
    follow_ep(nfst.states, root, leafs, visited, del);
    memset(visited, 0, sizeof(visit) * nfst.len);
    char buffer[2048];
    int pos = 0;
    int size = fread(buffer, 1, 2048, input);

    char cur_char;
    while (pos < size) {
        //print_leafs(leafs, "leafs.txt");
        cur_char = buffer[pos];
        // Iterate over active leaf nodes.
        for (int j = 0; j < leafs->len; ++j) {
            node* leaf = leafs->data[j];
            if (leaf == NULL) {
                continue;
            }
            step(cur_char, nfst.states, leaf, leafs2, visited, del);
        }

        // Swap leafs to the newly found ones, and reset the other vector.
        tmp = leafs;
        leafs = leafs2;
        leafs2 = tmp;
        leafs2->len = 0;

        // Prune path tree, and print whatever resides in the root node.
        for (int i = 0; i < del->len; ++i) {
            if (del->data[i] == NULL) {
                continue;
            }
            delete(del->data[i], &root);
        }
        del->len = 0;


        // Reset visited array.
        memset(visited, 0, sizeof(visit) * nfst.len);

        pos++;
        if (pos >= size) {
            fwrite(root->valuation->data, 1, root->valuation->len, output);
            root->valuation->len = 0;
            size = fread(buffer, 1, 2048, input);
            pos = 0;
        }
    }
    foutput(leafs, nfst.states, output);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        // Simple way to check actual size of a node struct.
        printf("Node size: %lu, State size: %lu\n", sizeof(node), sizeof(state));
        exit(23);
    }

    pool = nvector_create();

    int nlen;
    nfst_s* nfsts = parse(argv[1], &nlen);


    FILE* cur_ifile = stdin;
    FILE* cur_ofile, *next_ifile;
    pid_t childpid;

    for (int i = 0; i < nlen-1; ++i) {
        // Last iteration?
        int fd[2];
        if(pipe(fd) == -1) {
            perror("Failed call to pipe()");
            exit(1);
        }
        cur_ofile = fdopen(fd[1], "w");
        next_ifile = fdopen(fd[0], "r");

        if ((childpid = fork()) == -1) {
            perror("Failed fork()");
            exit(1);
        }

        // Child
        if (childpid == 0) {
            fclose(next_ifile);
            simulate(nfsts[i], cur_ifile, cur_ofile);
            exit(0);
        } else {
            fclose(cur_ofile);
            fclose(cur_ifile);
            cur_ifile = next_ifile;
        }
    }
    simulate(nfsts[nlen-1], cur_ifile, stdout);

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

