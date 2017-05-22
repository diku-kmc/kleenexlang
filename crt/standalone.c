#include <getopt.h>
#include "crt.h"

#define NUM_PHASES 1

static struct option long_options[] = {
   { "phase", required_argument, 0, 'p' },
   { "state", required_argument, 0, 's' },
   { 0, 0, 0, 0 }
};

static void printUsage(char *name)
{
  fprintf(stdout, "Normal usage: %s < infile > outfile\n", name);
  fprintf(stdout, "- \"%s\": reads from stdin and writes to stdout.\n", name);
  fprintf(stdout, "- \"%s -i\": prints compilation info.\n", name);
  fprintf(stdout, "- \"%s -t\": runs normally, but prints timing to stderr.\n", name);
  fprintf(stdout, "- \"%s -o\": prints state table.\n", name);
  fprintf(stdout, "- \"%s < infile > outfile -p n -s m\":\n\tStart processing input data from phase n state m.\n", name);
}

void print_callback(transducer_state* state) {

}

INLINE
int readnext(int minCount, int maxCount)
{
  // We can always take epsilon transitions
  if (minCount == 0) return 1;

  if (avail < maxCount)
    {
      int remaining = avail;
      memmove(&inbuf[INBUFFER_SIZE - remaining], &inbuf[INBUFFER_SIZE+in_cursor], remaining);
      in_cursor = -remaining;
      in_size = fread(&inbuf[INBUFFER_SIZE], 1, INBUFFER_SIZE, stdin);
    }
  if (avail < minCount)
    {
      return 0;
    }
  next = &inbuf[INBUFFER_SIZE+in_cursor];
  return 1;
}

void flush_outbuf()
{
  if (outbuf.bitpos % BUFFER_UNIT_BITS != 0)
    {
      outputconst(0, BUFFER_UNIT_BITS);
    }
  buf_flush(&outbuf);
}

INLINE
void outputconst(buffer_unit_t w, size_t bits)
{
  if (buf_writeconst(&outbuf, w, bits))
    {
      buf_flush(&outbuf);
    }
}


void run(int phase, int start_state)
{
  transducer_state* state = init();

  match(phase, start_state, state, &print_callback);

  flush_outbuf();
}

int main(int argc, char *argv[])
{
  bool do_timing = false;
  int c;
  int option_index = 0;
  int phase;
  int start_state = -1;
  bool do_phase = false;

  while ((c = getopt_long (argc, argv, "ihtp:s:", long_options, &option_index)) != -1)
  {
    switch (c)
    {
      case 'i':
        printCompilationInfo();
        return RETC_PRINT_INFO;
      case 't':
        do_timing = true;
        break;
      case 's':
        start_state = atoi(optarg);
        break;
      case 'p':
        phase = atoi(optarg);
        do_phase = true;
        break;
      case 'h':
      default:
        printUsage(argv[0]);
        return RETC_PRINT_USAGE;
    }
  }

  struct timeval time_before, time_after, time_result;
  long int millis;
  if(do_timing)
  {
    gettimeofday(&time_before, NULL);
  }

  if (do_phase)
  {
    run(phase, start_state);
  }
  else
  {
    // set up a pipeline
    // stdin -> prog --phase 1 -> prog --phase 2 -> ... -> prog --phase n -> stdout

    int orig_stdout = dup(STDOUT_FILENO);
    int pipes[NUM_PHASES-1][2];

    int i;
    for (i = 1; i < NUM_PHASES; i++)
    {
      if (i != 1) close(pipes[i-2][WRITE_FD]);

      if (pipe(pipes[i-1]) != 0)
      {
        fprintf(stderr, "Error creating pipe %d.", i);
        return 1;
      }
      dup2(pipes[i-1][WRITE_FD], STDOUT_FILENO);

      if (! fork())
      {
        close(orig_stdout);
        close(pipes[i-1][READ_FD]);

        run(i, start_state);
        return 0;
      }

      close(STDIN_FILENO);
      dup2(pipes[i-1][READ_FD], STDIN_FILENO);
    }

    #if NUM_PHASES>1
    close(pipes[NUM_PHASES-2][WRITE_FD]);
    dup2(orig_stdout, STDOUT_FILENO);
    #endif

    // Run last phase in-process
    run(NUM_PHASES, start_state);
  }

  if (do_timing)
  {
    gettimeofday(&time_after, NULL);
    timersub(&time_after, &time_before, &time_result);
    // A timeval contains seconds and microseconds.
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "time (ms): %ld\n", millis);
  }

  return 0;
}
