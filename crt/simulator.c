
#define NUM_NODES 100

typedef struct {
  unsigned long pkey;
  ptree *pleft;
  ptree *pright;
} ptree;


ptree* active_nodes[NUM_NODES] = { 0 };


void init(int num) {
  
}
