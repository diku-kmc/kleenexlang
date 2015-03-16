#include "../common.hpp"

#define REGEX "(a*\\n)"

// C++11 regex program corresponding to the "as" program.
// It matches the input string agains a* and outputs it.

int main(int argc, char *argv[]) {
  // std::regex::optimize: "Instructs the regular expression
  //   engine to make matching faster, with the potential cost
  //   of making construction slower. For example, this might
  //   mean converting a non-deterministic FSA to a deterministic FSA."
  PRE(regex::optimize)
  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    if (regex_match(buffer, pieces_match, re)) {
      fputs(pieces_match[1].str().c_str(), stdout);
    } else {
      MATCH_ERROR
    }
  }

  PRINT_TIMES
  
  return 0;
}
