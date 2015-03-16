#include "../common.hpp"

// RE2 program corresponding - approximately - to patho2
// output the string if it ends with a "b". 
string regex("(?:([a-z]*a)|([a-z]*b))?\n?");

#undef NCAP
#define NCAP 2

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
                  fputs(target[1].c_str(), stdout);
                  fputs("\n", stdout);
                }
                )
  PRINT_TIMES
  
  return 0;
}
