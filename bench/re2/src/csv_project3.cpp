#include "../common.hpp"

// RE2 program corresponding to csv_project3.has
// Projects away all columns except number 2 and 5.
// ([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\n

#ifdef USE_FGETS
  string regex("([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)\\n");
#else
  string regex("([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)");
#endif

#undef NCAP
#define NCAP 6

int main(int argc, char *argv[]) {
  SETOPTS
  // Pre-compile pattern
  uint64_t preCompile = getTimeMs();
  RE2 pattern(regex, options);

  // Initialize capture arguments
  RE2::Arg *args[NCAP];
  string target[NCAP];
  for (int i = 0; i < NCAP; i++) {
    args[i] = new RE2::Arg(&target[i]);
  }

  // START LINE-BASED TIMING
  uint64_t start = getTimeMs();
  FOR_EACH_LINE(
                bool match = RE2::FullMatchN(buffer, pattern, args, NCAP);
                if(!match) {
                  cerr << "match error on line " << line << endl;
                  cerr << buffer;
                  return 1;
                } else {
                  cout << target[1] << "\t" << target[4] << endl;
                }
                )
    
  uint64_t stop = getTimeMs();
  // END LINE-BASED TIMING

  cerr << "matching was successful." << endl;
  cerr << "compilation (ms): " << start - preCompile << endl;
  cerr << "matching (ms):    " << stop - start << endl;
  
  return 0;
}
