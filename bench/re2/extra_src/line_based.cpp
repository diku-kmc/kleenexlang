// Line based RE2 subgroup capturing
#include "../common.hpp"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <regex-file>\n", argv[0]);
        return 1;
    }
  
  // Read in filename
  FILE *re_f = fopen(argv[1], "r");
  string regex( read_all(re_f) );
  fclose(re_f);
  regex = rtrim(regex);


  // Pre-compile pattern
  uint64_t preCompile = getTimeMs();
  RE2 pattern(regex);

  // Initialize capture arguments
  RE2::Arg *args[NCAP];
  string target[NCAP];
  for (int i = 0; i < NCAP; i++) {
    args[i] = new RE2::Arg(&target[i]);
  }

  // START LINE-BASEDD TIMING
  uint64_t start = getTimeMs();
  uint64_t line = 0;
  // from man fgets:
  // "The newline, if any, is retained."
  while(fgets(buffer, LINE_LEN, stdin)) {
    line++;
    bool match = CAPTURE ? RE2::FullMatchN(buffer, pattern, args, NCAP)
                         : RE2::FullMatch(buffer, pattern);
    if(!match) {
      cerr << "match error on line " << line << endl;
      cerr << buffer;
      return 1;
    }
  }

  uint64_t stop = getTimeMs();
  // END LINE-BASED TIMING

  cout << "matching was successful." << endl;
  cout << "compilation (ms): " << start - preCompile << endl;
  cout << "matching (ms):    " << stop - start << endl;
  
  return 0;
}
