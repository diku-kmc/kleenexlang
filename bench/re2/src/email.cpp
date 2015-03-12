#include "../common.hpp"

// RE2 version of the email validator
// Like the others, it matches each line of input and outputs it if it is a "valid"
// e-mail address or skips it otherwise.

// Differences to the Hased version:
//    the \. has been turned into \\. because of c++ escaping
//    added \\n at the end, because the line iteration keeps the newlines
string regexprime("[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?");

#ifdef USE_FGETS
  string regex(regexprime + "\\n");
#else
  string regex(regexprime);
#endif

#define NCAP 0

int main(int argc, char *argv[]) {
  SETOPTS
  // Pre-compile pattern
  uint64_t preCompile = getTimeMs();
  RE2 pattern(regex, options);

  // NO CAPTURE ARGUMENTS!

  // START LINE-BASED TIMING
  uint64_t start = getTimeMs();
  FOR_EACH_LINE(
                bool match = RE2::FullMatch(buffer, pattern);
                if(match) {
                  #ifdef USE_FGETS
                    cout << buffer;
                  #else
                    cout << buffer << endl;
                  #endif
                }
                )

  uint64_t stop = getTimeMs();
  // END LINE-BASED TIMING

  cerr << "matching was successful." << endl;
  cerr << "compilation (ms): " << start - preCompile << endl;
  cerr << "matching (ms):    " << stop - start << endl;
  
  return 0;
}
