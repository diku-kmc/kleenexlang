// common re2 things

#include <stdio.h>
#include <re2/re2.h>
#include <iostream>
#include <string>
#include <sys/time.h>
#include <algorithm>
#include <inttypes.h>

using namespace std;

#ifndef USE_UTF8
  #define ENCODING_OPTION RE2::Options::EncodingLatin1
#else
  #define ENCODING_OPTION RE2::Options::EncodingUTF8
#endif

#define MATCH_ERROR                                             \
  fprintf(stderr, "match error on line %d\n", line);            \
  return 1;

#define PRE_COMPILE                             \
  uint64_t preCompile = getTimeMs();            \
  RE2 pattern(regex, options);

#define START_TIMING uint64_t start = getTimeMs();

#define PRINT_TIMES                                                     \
  uint64_t stop = getTimeMs();                                          \
  fprintf(stderr, "\ncompilation (ms): %" PRIu64 "\n", start - preCompile);  \
  fprintf(stderr, "matching (ms):    %" PRIu64 "\n", stop - start);

// Initialize capture arguments
#define INIT_RE2_CAPTURE_ARGS(N)                \
  RE2::Arg *args[N];                            \
  string target[N];                             \
  for (int i = 0; i < N; i++) {                 \
    args[i] = new RE2::Arg(&target[i]);         \
  }

// C-style fgets() is much much faster than C++-style getline(), so always use that.
#define BUFFER_SIZE (200*1024*1024)
char buffer[BUFFER_SIZE] = {0};

#define SETOPTS                                 \
  RE2::Options options;                         \
  options.set_dot_nl(true);                     \
  options.set_encoding(ENCODING_OPTION);


#define FOR_EACH_LINE(BODY)                     \
  int line = 0;                                 \
  while(fgets(buffer, sizeof(buffer), stdin)) { \
    line++;                                     \
    BODY                                        \
  }


// Size of the chunks we read in at a time
#define INPUT_BLOCK_SIZE (1024*1024)

// Number of capturing parentheses
#ifndef NO_CAPTURE
    #define CAPTURE true
#else
    #define CAPTURE false
#endif

/** Gets the current timestamp in millisecond resolution */
uint64_t getTimeMs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000 + tv.tv_sec * 1000;
}

