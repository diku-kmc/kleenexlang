#include "../common.hpp"

// C++11 regex program corresponding to the "email" program.
// It validates a list of email addresses.

// Differences from the regexes in the other email programs:
//   - all non-capturing parentheses (?: ) have been turned into ().
//     When they are present, we get an uncaught runtime "exception of
//     type std::__1::regex_error: An empty regex is not allowed in the POSIX grammar."
//   - the dot are escaped with two backslashes, in order not to clash with the C++ escaping.
#define REGEX "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?\n"

int main(int argc, char *argv[]) {

  // std::regex::optimize: "Instructs the regular expression
  //     engine to make matching faster, with the potential cost
  //     of making construction slower. For example, this might
  //     mean converting a non-deterministic FSA to a deterministic FSA."
  // std::regex::extended: "Use the extended POSIX regular expression
  //     grammar (grammar documentation)."
  // std::regex::nosubs: "When performing matches, all marked sub-expressions
  //     (expr) are treated as non-marking sub-expressions (?:expr). No
  //     matches are stored in the supplied std::regex_match structure
  //     and mark_count() is zero."
  PRE(regex::extended | regex::optimize | regex::nosubs)
  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    if (regex_match(buffer, pieces_match, re)) {
      fputs(pieces_match.str().c_str(), stdout);
    }
  }
  
  PRINT_TIMES
    
  return 0;
}
