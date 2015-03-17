// Common include file for C++11 regex tests.

#include <iostream>
#include <string>
#include <sys/time.h>
#include <stdio.h>
#include <algorithm>
#include <regex> // The C++11 default regex library
#include <inttypes.h>

using namespace std;

// Size of the buffer to read input into
#define BUFFER_SIZE (200*1024*1024)
// Filename buffer for regex
char buffer[BUFFER_SIZE] = {0};

// Maximum line length
#define LINE_LEN 100000000

#define PRE(OPTS)                               \
  uint64_t preCompile = getTimeMs();            \
  regex re(REGEX, OPTS);                        \
  cmatch pieces_match;                          \
  uint64_t start = getTimeMs();                 \
  int line = 0;

#define PRINT_TIMES                                                     \
  uint64_t stop = getTimeMs();                                          \
  fprintf(stderr, "\ncompilation (ms): %" PRIu64 "\n", start - preCompile);  \
  fprintf(stderr, "matching (ms):    %" PRIu64 "\n", stop - start);

#define MATCH_ERROR                                             \
  fprintf(stderr, "match error on line %d\n%s", line, buffer);  \
  return 1;

/** Gets the current timestamp in millisecond resolution */
uint64_t getTimeMs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000 + tv.tv_sec * 1000;
}
