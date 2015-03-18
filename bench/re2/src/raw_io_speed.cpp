#include <stdio.h>
#include <inttypes.h>
#include <iostream>
#include <sys/time.h>

#define BUFFER_SIZE (200*1024*1024)
char buffer[BUFFER_SIZE] = {0};
#define LINE_LEN 100000000

#define FOR_EACH_LINE(BODY)                     \
  int line = 0;                                 \
  while(fgets(buffer, LINE_LEN, stdin)) {       \
    line++;                                     \
    BODY                                        \
  }
/** Gets the current timestamp in millisecond resolution */
uint64_t getTimeMs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000 + tv.tv_sec * 1000;
}


// Measure the raw speed of how we read each line of input, without using any
// RE2 stuff at all.

int main(int argc, char *argv[]) {
  uint64_t start = getTimeMs();

  FOR_EACH_LINE(
                #ifdef USE_PRINTF
                fprintf(stdout, "%s\n", buffer);
                #else
                fputs(buffer, stdout);
                fputs("\n", stdout);
                #endif
                )
  uint64_t stop = getTimeMs();

  // Write "matching" for compatibility with other tests.
  fprintf(stderr, "\nmatching (ms): %" PRIu64 "\n", stop - start);
  
  return 0;
}
