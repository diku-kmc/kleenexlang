#include <string.h>
#include <stdio.h>

#define BUFFER_SIZE (200*1024*1024)
#define LINE_LEN 100000000

%%{
  machine flip_ab;
  action mk_a { fputs("a", stdout); }
  action mk_b { fputs("b", stdout); }

  main := ( 'a' $mk_b | 'b' $mk_a )* @{  };
}%%

%% write data;

char buffer[BUFFER_SIZE] = {0};
int main(int argc, char **argv) {
  int cs; // Current state
  
  while(fgets(buffer, LINE_LEN, stdin)) {
    char *p = &buffer[0];
    char *pe = p + strlen(buffer) + 1;
    %% write init;
    %% write exec;
  }
  return 0;
}
