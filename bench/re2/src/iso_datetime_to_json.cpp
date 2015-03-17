#include "../common.hpp"

string regexprime("((?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?");

string regex(regexprime + "\\n");

#define NCAP 7
#define OUT(S) fputs(S, stdout)

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
                  OUT("{'year'='"); OUT(target[0].c_str());
                  OUT("', 'month'='"); OUT(target[1].c_str());
                  OUT("', 'day'='"); OUT(target[2].c_str());
                  OUT("', 'hours'='"); OUT(target[3].c_str());
                  OUT("', 'minutes'='"); OUT(target[4].c_str());
                  OUT("', 'seconds'='"); OUT(target[5].c_str());
                  OUT("', 'tz'='"); OUT(target[6].c_str()); 
                  OUT("'}\n");
                }
                )

  PRINT_TIMES
  
  return 0;
}
