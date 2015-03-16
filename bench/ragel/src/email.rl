#include "../common.h"

/* From the Ragel manual:
        "To guard against this kind of problem one must ensure that the machine
         specification is divided up using boundaries that do not allow 
         ambiguities from one portion of the machine to the next."
*/


char *mark;

%%{
  machine email_fst;
  action mark {
         mark = p;
  }
  action print {
    fprintf(stdout, "%s", mark);
  }

  estr = /[a-z0-9!#$%&'*+/=?^_`{|}~\-]/;
  beforeAt = estr+ ('.' estr+)*;
  az09 = /[a-z0-9]/;
  az09dash = /[a-z0-9\-]*[a-z0-9]/;
  part = az09 az09dash? '.';
  afterAt = part+ az09 az09dash?;

  email = beforeAt '@' afterAt;  

  fb = /[^\n]*/;
  
  main := ((email > 10 >mark %print | (fb > 5)) '\n')* ;
}%%
/*
Call "mark" upon entry to the sub-machine "email".
Call "print" when exiting the sub-maching "email".
*/
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
