// common re2 things

#include <re2/re2.h>
#include <iostream>
#include <string>
#include <sys/time.h>
// #include <stdio.h>
#include <algorithm>

using namespace std;

#ifndef USE_UTF8
  #define ENCODING_OPTION RE2::Options::EncodingLatin1
#else
  #define ENCODING_OPTION RE2::Options::EncodingUTF8
#endif

#ifdef SYNC_STDIO
  #warning "Using sync_with_stdio(true)"
  #define SYNC ios_base::sync_with_stdio(true);
#else
  #warning "Using sync_with_stdio(false)"
  #define SYNC ios_base::sync_with_stdio(false);
#endif

#ifdef USE_FGETS
  #warning "Using fgets()"
  #define BUFFER_SIZE (200*1024*1024)
  char buffer[BUFFER_SIZE] = {0};
  #define READ_LINE fgets(buffer, LINE_LEN, stdin)
#else
  #warning "Using getline()"
  string buffer;
  #define READ_LINE getline(cin, buffer)
#endif


#define SETOPTS                                 \
  RE2::Options options;                         \
  options.set_dot_nl(true);                     \
  options.set_encoding(ENCODING_OPTION);


#define FOR_EACH_LINE(BODY)                     \
  SYNC                                          \
  int line = 0;                                 \
  while(READ_LINE) {                            \
    line++;                                     \
    BODY                                        \
  }


// Size of the chunks we read in at a time
#define INPUT_BLOCK_SIZE (1024*1024)

// Maximum line length
#define LINE_LEN 100000000
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

// /** Trims whitespace from end of a string
//   * Taken from http://stackoverflow.com/a/217605/79061 */
// static inline string &rtrim(string &s) {
//     s.erase(find_if(s.rbegin(), s.rend(), not1(ptr_fun<int, int>(isspace))).base(), s.end());
//     return s;
// }
