#include "../common.h"
// Ragel version of issuu_json2sql

int echo = 0;

%%{

  machine issuu_json2sql;

  action echo_on  { echo = 1; }
  action echo_off { echo = 0; }
  action print {
    if(echo) {
      fprintf(stdout, "%c", fc);
    } else {
      // fprintf(stdout, "[%c]", fc); 
    }
  }

  ws = [ \n]*;
  sep = ws ':' ws;
  keepComma = ws ',' %{ fputs(",", stdout); };
  dropComma = ws ',';
  someInt = '-'? [0-9]*;
  someString = /[^"\n]*/;
  qt = '\"' > { fputs("'", stdout); };
  stringReplaceQuotes = qt (someString > echo_on % echo_off) qt;
  fb = '\"' someString '\"' sep ( '\"' someString '\"'
                                | someInt
                                ) dropComma?;
  keyVal = ( '\"ts\"'                sep (someInt > echo_on % echo_off) keepComma
           | '\"visitor_uuid\"'      sep stringReplaceQuotes keepComma
           | '\"visitor_useragent\"' sep stringReplaceQuotes keepComma
           | '\"visitor_country\"'   sep stringReplaceQuotes dropComma
           | fb 
           );
  curl_start = '{' > { fputs("(", stdout); };
  curl_end   = '}' > { fputs(")", stdout); };
  object = curl_start (ws keyVal)* ws curl_end;


  json2sql = (object ws)* '\n';
  
  main := json2sql $ print;
}%%

%% write data;

int main(int argc, char **argv) {

  PRE;

  fputs("INSERT INTO issuu_log (ts, visitor_uuid, visitor_useragent, visitor_country) VALUES\n", stdout);
  
  int first = 1;

  while(fgets(buffer, sizeof(buffer), stdin)) {
    INIT_LINE;
    if(!first) {
      fputs(",\n", stdout);
    }
    first = 0;
    %% write init;
    %% write exec;

    if (p != pe) {
       FAIL;
    }
  }
  fputs(";\n", stdout);
  

  POST;
  
  return 0;
}
