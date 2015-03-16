#include "../common.hpp"

#define REGEX "([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\n"

// C++11 regex program corresponding to the "csv_project3" program.

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
      fputs(pieces_match[2].str().c_str(), stdout);
      fputs("\t", stdout);
      fputs(pieces_match[5].str().c_str(), stdout);
      fputs("\n", stdout);
    } else {
      MATCH_ERROR
    }
  }

  PRINT_TIMES
  
  return 0;
}
