#include "../common.h"

#define P(C) fputs(C, stdout);

int echo = 0;

%%{
  machine csv_project3;

  action echo_on  { echo = 1; }
  action echo_off { echo = 0; }
  action dump {
    if(echo) {
      fprintf(stdout, "%c", fc);
    }
  }

  col      = [^,\n]*;
  dropCol  = col > echo_off;
  keepCol  = col > echo_on % echo_off;
  row      = dropCol ','
             keepCol ',' > { P("\t") }
             dropCol ','
             dropCol ','
             keepCol ','
             dropCol;
  csvProj3 = (row '\n' > { P("\n") })*;

  main := csvProj3 $ dump;
}%%

%% write data;


int main(int argc, char **argv) {

  PRE;

  while(fgets(buffer, sizeof(buffer), stdin)) {
    INIT_LINE;
    %% write init;
    %% write exec;

    if(p != pe) {
      FAIL;
    }
  }

  POST;

  return 0;
}
