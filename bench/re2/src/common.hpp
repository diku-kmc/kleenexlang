// common re2 things

#include <re2/re2.h>
#include <iostream>
#include <string>
#include <sys/time.h>

using namespace std;

// Size of the buffer to read input into
#define BUFFER_SIZE (200*1024*1024)
// Size of the chunks we read in at a time
#define INPUT_BLOCK_SIZE (1024*1024)
// Filename buffer for regex
char buffer[BUFFER_SIZE] = {0};

// Maximum line length
#define LINE_LEN 100000000
// Number of capturing parentheses
#define NCAP 3
// Should the library capture groups, or simply do matching?
#ifndef NO_CAPTURE
    #define CAPTURE true
#else
    #define CAPTURE false
#endif

/** Read an entire stream into a string */
char *read_all(FILE *f) {
    size_t pos = 0, count = 0;
    do {
        count = fread(&buffer[pos], 1, INPUT_BLOCK_SIZE, f);
        pos += count;
    } while (count > 0);
    return buffer;
}

/** Gets the current timestamp in millisecond resolution */
uint64_t getTimeMs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000 + tv.tv_sec * 1000;
}

/** Trims whitespace from end of a string
  * Taken from http://stackoverflow.com/a/217605/79061 */
static inline string &rtrim(string &s) {
    s.erase(find_if(s.rbegin(), s.rend(), not1(ptr_fun<int, int>(isspace))).base(), s.end());
    return s;
}
