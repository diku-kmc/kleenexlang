#include "../common.hpp"

// C++11 regex program corresponding to the "patho2" program.

#define REGEX "(([a-z]*a)|([a-z]*b))?\n?"

int main(int argc, char *argv[]) {
  // Pre-compile pattern
  uint64_t preCompile = getTimeMs();

  // std::regex::optimize: "Instructs the regular expression
  //     engine to make matching faster, with the potential cost
  //     of making construction slower. For example, this might
  //     mean converting a non-deterministic FSA to a deterministic FSA."
  // std::regex::extended: "Use the extended POSIX regular expression
  //     grammar (grammar documentation)."
  regex re(REGEX, regex::extended | regex::optimize);
  cmatch pieces_match;
  
  // START LINE-BASED TIMING
  uint64_t start = getTimeMs();
  uint64_t line = 0;

  // from man fgets:
  // "The newline, if any, is retained."
  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    if (regex_match(buffer, pieces_match, re)) {
      cout << pieces_match[3] << endl;
    } else {
      cerr << "match error on line " << line << endl;
      return 1;
    }
  }

  uint64_t stop = getTimeMs();
  // END LINE-BASED TIMING

  cerr << endl << "matching was successful." << endl;
  cerr << "compilation (ms): " << start - preCompile << endl;
  cerr << "matching (ms):    " << stop - start << endl;
  
  return 0;
}
