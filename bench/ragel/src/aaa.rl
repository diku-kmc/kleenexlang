#include "../common.h"

char *mark;

%%{
  machine aaa_fst;
  action put_0 { fputs("0", stdout); }
  action put_1 { fputs("1", stdout); }

  seven_as = 'aaaaaaa';
  three_as = 'aaa';

  main := (seven_as > 10 % put_0 | three_as > 5 % put_1)*;
}%%

%% write data;



int main(int argc, char **argv) {

  PRE

  while(fgets(buffer, sizeof(buffer), stdin)) {
    char *p = &buffer[0];
    char *pe = p + strlen(buffer) + 1;
    %% write init;
    %% write exec;
  }

  POST

  return 0;
}
