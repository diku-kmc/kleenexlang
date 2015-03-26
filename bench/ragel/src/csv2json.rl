#include "../common.h"

#define P(C) fputs(C, stdout);
#define INDENT fputs("   ", stdout);
#define ON echo = 1;
#define OFF echo = 0;

int echo = 0;

%%{
  machine csv2json;

  action dump {
    if(echo) {
      fprintf(stdout, "%c", fc);
    }
  }

  ipVal     = ([0-9]{1,3} '.' [0-9]{1,3} '.' [0-9]{1,3} '.' [0-9]{1,3}) > { P("\"") } % { P("\"") };
  stringVal = [^,\n]* > { P("\"") } % { P("\"") };
  numVal    = [0-9]+;
  row       = numVal    > { ON; INDENT; P("\"id\"         : "); } % { OFF; P(",\n"); } ','
              stringVal > { ON; INDENT; P("\"first_name\" : "); } % { OFF; P(",\n"); } ','
              stringVal > { ON; INDENT; P("\"last_name\"  : "); } % { OFF; P(",\n"); } ','
              stringVal > { ON; INDENT; P("\"email\"      : "); } % { OFF; P(",\n"); } ','
              stringVal > { ON; INDENT; P("\"country\"    : "); } % { OFF; P(",\n"); } ','
              ipVal     > { ON; INDENT; P("\"ip\"         : "); } % { OFF; P("\n"); };
  csv2json  = row > { P("{\n") } % { P("}\n") } '\n';
    
  main := csv2json $ dump;
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
