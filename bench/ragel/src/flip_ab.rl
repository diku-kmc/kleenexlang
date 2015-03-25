#include "../common.h"
// Ragel implementation of the flip_ab

char outchar;
%%{
  machine flip_ab;
  action out {
    fputs(&outchar, stdout);
  }

  a = 'a' > { outchar = 'b'; };
  b = 'b' > { outchar = 'a'; };
  
  main := (a | b)* $ out;
}%%

%% write data;

int main(int argc, char **argv) {

  PRE;
  
  while(fgets(buffer, sizeof(buffer), stdin)) {
    INIT_LINE;
    %% write init;
    %% write exec;

    fputc('\n', stdout);

    if(p+1 != pe) {
      FAIL;
    }
  }

  POST;
  
  return 0;
}
