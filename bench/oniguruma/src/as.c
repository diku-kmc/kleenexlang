// C/Oniguruma version of the "as" program

#include "../common.h"

#define REGEX "a*\n"

int main(int argc, char* argv[]) {

  PRE

  while(fgets(buffer, sizeof(buffer), stdin)) {
    lno++;
    end   = buffer + strlen((char* )buffer);
    start = buffer;
    r = onig_match(reg, buffer, end, start, region, ONIG_OPTION_NONE);

    if (r < 0) {
      fprintf(stderr, "match error on line %d\n", lno);
      return 1;
    } else {
      int l = region->end[0] - region->beg[0];
      fprintf(stdout, "%*.*s", l, l, buffer);
    }
  }

  PRINT_TIMES
  
  return 0;
}
