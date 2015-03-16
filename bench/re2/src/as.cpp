#include "../common.hpp"

// RE2 implementation of as
// It matches the input string agains a* and outputs it.
string regex("(a*\\n)");

#define NCAP 1

int main(int argc, char *argv[]) {
  SETOPTS
    
  PRE_COMPILE

  INIT_RE2_CAPTURE_ARGS(NCAP)

  START_TIMING

  FOR_EACH_LINE(
                bool match = RE2::FullMatchN(buffer, pattern, args, NCAP);
                if(!match) {
                  MATCH_ERROR
                } else {
                  fputs(target[0].c_str(), stdout);
                }
                )

  PRINT_TIMES
  
  return 0;
}
