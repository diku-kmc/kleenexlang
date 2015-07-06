#include "../common.hpp"

#define REGEX "([0-9]{1,3})((?:[0-9]{3})*)\n"
#define REGEX2 "([0-9]{3})"

#define OUT(S) fputs(S, stdout)

int main(int argc, char *argv[]) {
  // std::regex::optimize: "Instructs the regular expression
  //   engine to make matching faster, with the potential cost
  //   of making construction slower. For example, this might
  //   mean converting a non-deterministic FSA to a deterministic FSA."
  PRE2(regex::optimize)

  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    if (regex_match(buffer, pieces_match, re)) {
        OUT(pieces_match[1].str().c_str());
        if (pieces_match[2].length() > 0) {
            OUT( regex_replace(pieces_match[2].str(), re2, string(",$1")).c_str() );
        }
        OUT("\n");
    } else {
      MATCH_ERROR
    }
  }

  PRINT_TIMES
  
  return 0;
}
