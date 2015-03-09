// C/Oniguruma version of the "csv_project3" program

#include "../common.h"

#define RE "([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\n"

int main(int argc, char* argv[]) {
  int r;
  char *start, *end;
  regex_t* reg;
  OnigErrorInfo einfo;
  OnigRegion *region;

  uint64_t time_pre_compile = getTimeMs();
  
  r = onig_new(&reg, RE, RE + strlen((char* )RE),
               ONIG_OPTION_DEFAULT,
               ONIG_ENCODING_UTF8,
               ONIG_SYNTAX_POSIX_EXTENDED,
               &einfo);
  if (r != ONIG_NORMAL) {
    char s[ONIG_MAX_ERROR_MESSAGE_LEN];
    onig_error_code_to_str(s, r, &einfo);
    fprintf(stderr, "ERROR: %s\n", s);
    return -1;
  }
  int lno = 0;
  uint64_t time_start = getTimeMs();
  
  region = onig_region_new();
  while(fgets(buffer, LINE_LEN, stdin)) {
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

  onig_region_free(region, 1 /* 1:free self, 0:free contents only */);
  onig_free(reg);
  onig_end();

  uint64_t time_end = getTimeMs();

  fprintf(stderr, "\ncompilation (ms): %llu\n", time_start - time_pre_compile);
  fprintf(stderr, "matching (ms):    %llu\n", time_end - time_start);
  
  return 0;
}
