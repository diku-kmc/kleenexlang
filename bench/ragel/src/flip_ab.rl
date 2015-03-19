#include "../common.h"
// Ragel implementation of the flip_ab

%%{
  machine flip_ab;
  action mk_a { fputs("a", stdout); }
  action mk_b { fputs("b", stdout); }

  main := ( 'a' $mk_b | 'b' $mk_a )* @{  };
}%%

%% write data;

int main(int argc, char **argv) {

  PRE;
  
  while(fgets(buffer, sizeof(buffer), stdin)) {
    char *p = &buffer[0];
    char *pe = p + strlen(buffer) + 1;
    %% write init;
    %% write exec;
  }

  POST;
  
  return 0;
}
