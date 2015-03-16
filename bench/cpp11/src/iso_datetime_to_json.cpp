#include "../common.hpp"

#define REGEX "(([1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](2[0-3]|[0-1][0-9]):[0-5][0-9])?\n"

#define OUT(S) fputs(S, stdout)

int main(int argc, char *argv[]) {
  // std::regex::optimize: "Instructs the regular expression
  //   engine to make matching faster, with the potential cost
  //   of making construction slower. For example, this might
  //   mean converting a non-deterministic FSA to a deterministic FSA."
  // std::regex::extended: "Use the extended POSIX regular expression
  //   grammar (grammar documentation)." 
  PRE(regex::extended | regex::optimize)

  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    if (regex_match(buffer, pieces_match, re)) {
      OUT("{'year'='")      ; OUT(pieces_match[1].str().c_str());
      OUT("', 'month'='")   ; OUT(pieces_match[3].str().c_str());
      OUT("', 'day'='")     ; OUT(pieces_match[4].str().c_str());
      OUT("', 'hours'='")   ; OUT(pieces_match[5].str().c_str());
      OUT("', 'minutes'='") ; OUT(pieces_match[6].str().c_str());
      OUT("', 'seconds'='") ; OUT(pieces_match[7].str().c_str());
      OUT("', 'tz'='")      ; OUT(pieces_match[8].str().c_str()); 
      OUT("'}\n");
    } else {
      MATCH_ERROR
    }
  }

  PRINT_TIMES
  
  return 0;
}
