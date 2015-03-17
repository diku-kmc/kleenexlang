// C/Oniguruma version of the "csv_project3" program

#include "../common.h"

#define REGEX "([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\n"

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
      int l1 = region->end[2] - region->beg[2];
      int l2 = region->end[5] - region->beg[5];
      fprintf(stdout, "%.*s\t%.*s\n", l1, &buffer[region->beg[2]], l2, &buffer[region->beg[5]]);
    }
  }

  PRINT_TIMES
  
  return 0;
}
