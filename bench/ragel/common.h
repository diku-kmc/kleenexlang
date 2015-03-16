#include <string.h>
#include <stdio.h>
#include <sys/time.h>
#include <stdint.h>

#define BUFFER_SIZE (200*1024*1024)
char buffer[BUFFER_SIZE] = {0};

#define PRE                                     \
  int cs;                                       \
  char *eof;                                    \
  uint64_t start = getTimeMs();

#define POST                                                \
  uint64_t stop = getTimeMs();                              \
  fprintf(stderr, "\nmatching (ms): %llu\n", stop - start);


uint64_t getTimeMs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000 + tv.tv_sec * 1000;
}
