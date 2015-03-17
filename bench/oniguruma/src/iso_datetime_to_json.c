#include "../common.h"

#define REGEX "(([1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](2[0-3]|[0-1][0-9]):[0-5][0-9])?\n"


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
#define R(N) int l##N = region->end[N] - region->beg[N]
#define B(N) l##N, &buffer[region->beg[N]]
      R(1); R(3); R(4); R(5); R(6); R(7); R(8);
      fprintf(stdout, "{'year'='%.*s', 'month'='%.*s', 'day'='%.*s', 'hours'='%.*s', 'minutes'='%.*s', 'seconds'='%.*s', 'tz'='%.*s'}\n", B(1), B(3), B(4), B(5), B(6), B(7), B(8));
    }
  }

  PRINT_TIMES
  
  return 0;
}
