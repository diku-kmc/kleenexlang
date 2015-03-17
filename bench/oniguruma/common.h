// Common stuff for the Oniguruma tests.

#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>
#include <string.h>
#include <inttypes.h>
#include "oniguruma.h"

#define PRE                                                 \
  int r;                                                    \
  char *start, *end;                                        \
  regex_t* reg;                                             \
  OnigErrorInfo einfo;                                      \
  OnigRegion *region;                                       \
  uint64_t time_pre_compile = getTimeMs();                  \
  r = onig_new(&reg, REGEX, REGEX + strlen((char* )REGEX),  \
               ONIG_OPTION_DEFAULT,                         \
               ONIG_ENCODING_UTF8,                          \
               ONIG_SYNTAX_POSIX_EXTENDED,                  \
               &einfo);                                     \
  if (r != ONIG_NORMAL) {                                   \
    char s[ONIG_MAX_ERROR_MESSAGE_LEN];                     \
    onig_error_code_to_str(s, r, &einfo);                   \
    fprintf(stderr, "ERROR: %s\n", s);                      \
    return -1;                                              \
  }                                                         \
  int lno = 0;                                              \
  uint64_t time_start = getTimeMs();                        \
  region = onig_region_new();

#define PRINT_TIMES                                                     \
  onig_region_free(region, 1);                                          \
  onig_free(reg);                                                       \
  onig_end();                                                           \
  uint64_t time_end = getTimeMs();                                      \
  fprintf(stderr, "\ncompilation (ms): %" PRIu64 "\n", time_start - time_pre_compile); \
  fprintf(stderr, "matching (ms):    %" PRIu64 "\n", time_end - time_start);

  

// Size of the buffer to read input into
#define BUFFER_SIZE (200*1024*1024)
// Size of the chunks we read in at a time
#define INPUT_BLOCK_SIZE (1024*1024)
// Filename buffer for regex
char buffer[BUFFER_SIZE] = {0};

/** Gets the current timestamp in millisecond resolution */
uint64_t getTimeMs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000 + tv.tv_sec * 1000;
}

