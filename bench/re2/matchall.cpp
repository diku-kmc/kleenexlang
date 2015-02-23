// From Kristoffer and Sebastians repository.

#include <re2/re2.h>
#include <iostream>
#include <string>
#include <sys/time.h>

using std::string;
// using namespace std;

// Size of the buffer to read input into
#define BUFFER_SIZE (200*1024*1024)
char buffer[BUFFER_SIZE] = {0};

// Filename buffer for regex
char filename[10240] = {0};

// Size of the chunks we read in at a time
#define INPUT_BLOCK_SIZE (1024*1024)

// Number of capturing parentheses
#define NCAP 3

// Should the library capture groups, or simply do matching?
#ifndef NO_CAPTURE
    #define CAPTURE true
#else
    #define CAPTURE false
#endif

/** Read an entire stream into a string */
char *read_all(FILE *f) {
    size_t pos = 0, count = 0;
    do {
        count = fread(&buffer[pos], 1, INPUT_BLOCK_SIZE, f);
        pos += count;
    } while (count > 0);
    return buffer;
}

/** Gets the current timestamp in microsecond resolution */
uint64_t getTimeUs() {
    struct timeval tv;
    gettimeofday(&tv, NULL);

    return tv.tv_usec + tv.tv_sec * 1000 * 1000;
}

/** Trims whitespace from end of a string
  * Taken from http://stackoverflow.com/a/217605/79061 */
static inline std::string &rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
    return s;
}

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
    uint64_t preCompile = getTimeUs();
    RE2 pattern(regex);

    // Initialize capture arguments
    RE2::Arg *args[NCAP];
    string target[NCAP];
    for (int i = 0; i < NCAP; i++) {
        args[i] = new RE2::Arg(&target[i]);
    }

    // START TIMING
    uint64_t start = getTimeUs();

    char *data = read_all(stdin);

    uint64_t data_read = getTimeUs();

    bool match = CAPTURE ? RE2::FullMatchN(data, pattern, args, NCAP)
                         : RE2::FullMatch(data, pattern);

    uint64_t stop = getTimeUs();
    // END TIMING

    std::cout << "matching was " << (match ? "" : "un") << "successful." << std::endl;
    std::cout << start - preCompile << " uS spent on compilation" << std::endl;
    std::cout << stop - start << " uS spent on matching" << std::endl;
    std::cout << "of this, " << data_read - start << " uS was spent on reading in the data" << std::endl;
}
