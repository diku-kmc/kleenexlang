#include "../common.hpp"

// RE2 program corresponding to csv_project3
// Projects away all columns except number 2 and 5.
// ([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\n

string regex("([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\\n");

#define NCAP 6

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
                  fputs("\t", stdout);
                  fputs(target[4].c_str(), stdout);
                  fputs("\n", stdout);
                }
                )
    
  PRINT_TIMES
  
  return 0;
}
