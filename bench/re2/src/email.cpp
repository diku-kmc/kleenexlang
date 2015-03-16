#include "../common.hpp"

// RE2 version of the email validator
// Like the others, it matches each line of input and outputs it if it is a "valid"
// e-mail address or skips it otherwise.

// Differences to the Kleenex version:
//    the \. has been turned into \\. because of c++ escaping
//    added \\n at the end, because the line iteration keeps the newlines
string regexprime("[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?");

string regex(regexprime + "\\n");

#define NCAP 0

int main(int argc, char *argv[]) {
  SETOPTS

  PRE_COMPILE

  INIT_RE2_CAPTURE_ARGS(NCAP)

  START_TIMING

  FOR_EACH_LINE(
                bool match = RE2::FullMatch(buffer, pattern);
                if(match) {
                  fputs(buffer, stdout);
                }
                )

  PRINT_TIMES
  
  return 0;
}
