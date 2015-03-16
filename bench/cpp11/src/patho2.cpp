#include "../common.hpp"

// C++11 regex program corresponding to the "patho2" program.

#define REGEX "(([a-z]*a)|([a-z]*b))?\n?"

int main(int argc, char *argv[]) {
  // std::regex::optimize: "Instructs the regular expression
  //     engine to make matching faster, with the potential cost
  //     of making construction slower. For example, this might
  //     mean converting a non-deterministic FSA to a deterministic FSA."
  // std::regex::extended: "Use the extended POSIX regular expression
  //     grammar (grammar documentation)."
  PRE(regex::extended | regex::optimize)
  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    if (regex_match(buffer, pieces_match, re)) {
      fputs(pieces_match[3].str().c_str(), stdout);
      fputs("\n", stdout);
    } else {
      MATCH_ERROR
    }
  }

  PRINT_TIMES
  
  return 0;
}
