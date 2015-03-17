// C/Oniguruma version of the "email" program

#include "../common.h"

#define REGEX "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?\n"

int main(int argc, char* argv[]) {

  PRE
  
  while(fgets(buffer, sizeof(buffer), stdin)) {
    lno++;
    end   = buffer + strlen((char* )buffer);
    start = buffer;
    r = onig_match(reg, buffer, end, start, region, ONIG_OPTION_NONE);

    if (r < 0) {
      // Do nothing
    } else {
      int l = region->end[0] - region->beg[0];
      fprintf(stdout, "%*.*s", l, l, buffer);
    }
  }

  PRINT_TIMES

  return 0;
}
