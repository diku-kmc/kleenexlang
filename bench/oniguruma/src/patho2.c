// C/Oniguruma version of the "patho2" program

#include "../common.h"

#define REGEX "^(([a-z]*a)|([a-z]*b))?$"

int main(int argc, char* argv[]) {

  PRE
  
  while(fgets(buffer, sizeof(buffer), stdin)) {
    lno++;
    end   = buffer + strlen((char* )buffer);
    start = buffer;
    r = onig_match(reg, buffer, end, start, region, ONIG_OPTION_NONE);

    if (r < 0) {
      fprintf(stderr, "matching error on line %d\n", lno);
      return 1;
    } else {
      int l = region->end[3] - region->beg[3];
      fprintf(stdout, "%*.*s\n", l, l, buffer);
    }
  }

  PRINT_TIMES

  return 0;
}
