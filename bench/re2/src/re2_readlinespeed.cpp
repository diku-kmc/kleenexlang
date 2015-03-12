#include "../common.hpp"

// Measure the raw speed of how we read each line of input, without using any
// RE2 stuff at all.

int main(int argc, char *argv[]) {

  // START LINE-BASED TIMING
  uint64_t start = getTimeMs();

  FOR_EACH_LINE(
                cout << buffer << endl;
                )
  uint64_t stop = getTimeMs();
  // END LINE-BASED TIMING

  cerr << "matching (ms):    " << stop - start << endl;
  
  return 0;
}
