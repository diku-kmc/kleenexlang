#include "../common.hpp"

using namespace std;

// Number of capturing parentheses
#define NCAP 3

// Should the library capture groups, or simply do matching?
#ifndef NO_CAPTURE
    #define CAPTURE true
#else
    #define CAPTURE false
#endif

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

    // START TIMING
    uint64_t start = getTimeMs();

    char *data = read_all(stdin);

    uint64_t data_read = getTimeMs();

    bool match = CAPTURE ? RE2::FullMatchN(data, pattern, args, NCAP)
                         : RE2::FullMatch(data, pattern);

    uint64_t stop = getTimeMs();
    // END TIMING

    cout << "matching was " << (match ? "" : "un") << "successful." << endl;
    cout << "compilation (ms):  " << start - preCompile << endl;
    cout << "matching (ms):     " << stop - start << endl;
    cout << "data reading (ms): " << data_read - start << endl;
}
