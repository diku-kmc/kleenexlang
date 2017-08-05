//
//  dispatcher.c
//
//
//  Created by Jonas Jørgensen on 26/04/2017.
//
//

#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "thr_pool.h"
#include "crt.h"

/* Macro for toggling debugging related code */
//#define DEBUGGING

/* Exit codes for non-successful evaluation */
#define RETC_PRINT_USAGE      1
#define RETC_PRINT_ERROR      2

/* Timing flags */
#define TIME_ALL     0x01   // Flag for timing full execution
#define TIME_DATA    0x02   // Flag for timing mapping of input to memory
#define TIME_FST     0x04   // Flag for timing data processing
#define TIME_OUTPUT  0x08   // Flag for timing writing output

/* Default values */
#define DEFAULT_SUFFIX_LENGTH     256
#define DEFAULT_NUMBER_OF_CHUNKS  sysconf(_SC_NPROCESSORS_ONLN) // Use number of online processors as default

/*
 * Struct representing a job to be executed.
 * Contains relevant information for adding
 * a job to the pool queue, and to keep track
 * of the queued job.
 */
typedef struct job_t {
  struct job_t *next;
  int start_state;
  int chunk;
  int phase;
  bool completed;
  bool cancled;
  job_id job;
  transducer_state *tstate;
} job_t;


/*
 * Struct containing data relevant for
 * job generation.
 */
typedef struct {
  int phase;
  int chunk;
  size_t offset;
  size_t chunk_lengt;
  size_t suffix_length;
} task_gen_arg_t;


/* Mutexes */
static pthread_mutex_t job_list_lock = PTHREAD_MUTEX_INITIALIZER; /* Protects job list */
static pthread_mutex_t path_lock = PTHREAD_MUTEX_INITIALIZER; /* Protects job list */

/* Global variables */
thr_pool_t *pool;           // Thread Pool

unsigned char* target;      // Content of target file
long target_size;           // Size of target file

long num_of_chunks;         // Number of chunks to split input data into

transducer_state **** outputmap;  // Table of handles used to reference transducer
                                  // states generated during data processing

job_t **joblist;            // Job list for keeping track of active jobs

transducer_state **path;    // List of tranducer states that, when combined,
                            // representes the ouput

struct timeval time_start;  // Wall time after processing argument options
struct timeval time_data;   // Wall time after mapping input data to memory
struct timeval time_fst;    // Wall time after processing input data
struct timeval time_output; // Wall time after writing output

/* Function declarations */
void print_usage(char *);
void print_timeing(int);
void enqueue_job(job_t *);

/* Thread jobs */
void *calculate(void *);
void *generate_tasks(void *);
void *update_path_and_clean(void *);


/*
 * Called by a worker thread on return from a job.
 */
static void
cleanup_joblist(void *_)
{
  (void) pthread_mutex_unlock(&job_list_lock);
}

/*
 * Called by a worker thread on return from a job.
 */
static void
cleanup_paht(void *_)
{
  (void) pthread_mutex_unlock(&path_lock);
}

/*
 * Checks if a specific job is part of the
 * path.
 *
 * If so, cancle other jobs on same chunk,
 * and add next job to the path.
 */
void *update_path_and_clean(void *arg)
{
  job_t *job = (job_t *)arg;
  job_t *head = NULL, *next = NULL;


  (void) pthread_mutex_lock(&job_list_lock);
  pthread_cleanup_push(cleanup_joblist, NULL);

  /* Check that job has finished */
  // TODO: Is this check needed?
  if (path[job->chunk] != job->tstate) {
    (void) pthread_mutex_lock(&path_lock);
    pthread_cleanup_push(cleanup_paht, NULL);

    /* Else, terminate competing jobs */
    for (head = joblist[job->chunk];
         head != NULL;)
    {
      next = head->next;
      if (head->job != job->job) {
        if(!job->completed) {
          thr_pool_dequeue(pool, job->job);
        }
        free_state(job->tstate);
        free(job);
      }
      head = next;
    }

    if (job->completed && job->tstate->nextPtr != NULL) {
      path[job->chunk+1] = *(job->tstate->nextPtr);
    }

    if (job->chunk < num_of_chunks-1) {
      for (head = joblist[job->chunk+1];
           head != NULL;
           head = head->next)
      {
        thr_pool_queue_prioritized(pool, update_path_and_clean, head);
      }

    }
    joblist[job->chunk] = NULL;

    (void) pthread_mutex_unlock(&path_lock);
    pthread_cleanup_pop(false);
  }
  (void) pthread_mutex_unlock(&job_list_lock);
  pthread_cleanup_pop(false);
  return (0);
}

/**
 *    Maps file into memory.
 *    Protection level is set to read to prevent accidental changes.
 *    We use the MAP_FILE and MAP_SHARED flags to prevent the data
 *    from being copied when we spawn a child process.
 */
void init_target(char* file_name)
{
  FILE *f = fopen(file_name, "r");
  fseek(f, 0, SEEK_END);
  target_size = ftell(f);
  fclose(f);

  int fd = open(file_name, O_RDONLY);
  target = mmap(NULL, target_size + 1, PROT_READ, MAP_FILE | MAP_PRIVATE, fd, 0);
}

buffer_unit_t *output_pos(transducer_state * tstate) {
  return tstate->outbuf->data + tstate->output_cursor;
}


void output(transducer_state *tstate) {
  symbol *sym;
  size_t position;
  int chunk = 0;

  if (!tstate) return;

  /* Handle fail state */
  if (tstate->curr_state == -1
      || (tstate->nextPtr == NULL && ! state_table[0][tstate->curr_state].accepting)) {
    for (chunk = 0; chunk < num_of_chunks && path[chunk] != tstate && path[chunk] != NULL; chunk++);
    position = (target_size / num_of_chunks) * chunk + tstate->inbuf->cursor;
    fprintf(stderr, "Match error at input symbol %zu, (next char: %u)!\n", position, tstate->inbuf->next[0]);
    return;
  }

  for (sym = tstate->outbuf->symbols;
       sym != NULL;
       sym = sym->next) {

    /* Print from output until position of next symbol */
    tstate->output_cursor += fprintf(stdout, "%.*s",
                                     (int)(sym->position/BUFFER_UNIT_SIZE - tstate->output_cursor),
                                     output_pos(tstate));

    /* Print content of symbol */
    fprintf(stdout, "%s", tstate->src->buffers[sym->reg]->data);
  }

  /* Print remaining content */
  tstate->output_cursor += fprintf(stdout, "%s", output_pos(tstate));


  /* Delete all symbols */
  sym = tstate->outbuf->symbols;
  while (sym != NULL) {
    sym = sym->next;
    free(tstate->outbuf->symbols);
    tstate->outbuf->symbols = sym;
  }

  if (tstate->nextPtr != NULL) {
    /* Set current state as source for next state */
    (*tstate->nextPtr)->src = tstate;
    output(*tstate->nextPtr);
  }
}

void init_outputmap()
{
  int p, c;

  outputmap = malloc(sizeof(transducer_state*) * phases);

  for (p = 0; p < phases; p++) {
    outputmap[p] = malloc(sizeof(transducer_state*) * num_of_chunks);
    for (c = 0 ; c < num_of_chunks; c++)
    {
      outputmap[p][c] = calloc(state_count[p], sizeof(transducer_state*));
    }
  }
}

/*
 *    Cancel all queued and running jobs.
 *
 *    This is inteted to terminate the program
 *    if a fail state is reached during output generation.
 */
void abort_process() {
  thr_pool_destroy(&pool);
}


/*
 *    Generates jobs for a specified chunk.
 *    A suffix analysis is made to identify relevant start states.
 */
void *generate_tasks(void *arg) {
  int phase, chunk, start_state, *started, i;
  size_t length, offset, suffix_length;
  unsigned char *suffix, *start;
  task_gen_arg_t *info = (task_gen_arg_t *)arg;


  phase = info->phase;
  chunk = info->chunk;
  suffix_length = info->suffix_length;
  length = info->chunk_lengt;
  offset = info->offset;

  free(info);

  suffix = target + offset - suffix_length;
  start = target + offset;
  started = calloc(sizeof(start_state), state_count[phase]);

  for (i = 0; i < state_count[phase]; i++) {
    start_state = silent_match(phase+1, i, suffix, suffix_length);
    if (start_state < 0) continue; /* We do not start from a fail state */
    if (! started[start_state]) {
      //CREATE JOB
      job_t * task = malloc(sizeof(job_t));
      task->cancled = false;
      task->completed = false;
      task->chunk = chunk;
      task->next = NULL;
      task->phase = phase;
      task->tstate = init(start, length, false, true);
      task->tstate->curr_state = start_state;
      task->tstate->output_cursor = 0;
      enqueue_job(task);
#ifdef DEBUGGING
      fprintf(stdout, "Created job: phase: %i, chunk: %i, state: %i\n", phase, chunk, start_state);
#endif
      started[start_state] = 1;
      outputmap[phase][chunk][start_state] = task->tstate;
    }
  }

  free(started);
  return (0);
}


/*
 *    Function for processing a data chunk.
 */
void *calculate(void *arg)
{
  int phase, chunk;
  transducer_state *tstate;
  job_t * task = (job_t *)arg;
#ifdef DEBUGGING
  /* Store start state when debugging */
  int start_state = task->tstate->curr_state;
#endif

  /* Check if job has already been processed */
  /* NB! this should never happen */
  if (task->completed) return (0);

  /* Check if job has been cancled */
  if (task->cancled)
  {
    task = NULL;
    return (0);
  }

  phase = task->phase;
  chunk = task->chunk;
  tstate = task->tstate;

  /*
   * Kleenex phases are 1 indexed,
   * but everything else is 0 inexed.
   * Thus +1 on phase number.
   */
  if (chunk == num_of_chunks-1) {
    match(phase+1, tstate, NULL, true);
  } else {
    match(phase+1, tstate, NULL, false);
  }

  /*
   * If this is not the final chunk, then point
   * 'nextPtr' to the subsequent transducer state.
   */
  if (chunk < num_of_chunks-1)
  {
    tstate->nextPtr = &outputmap[phase][chunk+1][tstate->curr_state];
  }

  task->completed = 1;

  (void) pthread_mutex_lock(&path_lock);
  pthread_cleanup_push(cleanup_paht, NULL);

  if (path[chunk] == tstate)
  {
    if (tstate->nextPtr != NULL) {
      path[chunk+1] = *(tstate->nextPtr);
      thr_pool_queue_prioritized(pool, update_path_and_clean, task);
    }
  }

  (void) pthread_mutex_unlock(&path_lock);
  pthread_cleanup_pop(false);

#ifdef DEBUGGING
  /* If debugging, print phase, chunk,  */
  fprintf(stdout, "Job: phase: %i, chunk: %i, from -> to: %i -> %i\n", phase, chunk, start_state, tstate->curr_state);
#endif

  // TODO: SIGNAL THAT WE CAN COMBINE OUTPUT
  task = NULL;

  return (0);
}


/*
 *    Adds a job to the pool queue and registers it in the joblist
 */
void enqueue_job(job_t *task)
{
  (void) pthread_mutex_lock(&job_list_lock);
  pthread_cleanup_push(cleanup_joblist, NULL);

  task->next = joblist[task->chunk];
  joblist[task->chunk] = task;
  thr_pool_queue(pool, calculate, task, &task->job);

  (void) pthread_mutex_unlock(&job_list_lock);
  pthread_cleanup_pop(false);
}

/*
 * Controller for handling multiple data chunks.
 */
void run_multi_chunk(int suffix_len, int nthreads) {
  int i;
  job_t *init_task;

  /* SET UP */
  long chunk_size = target_size / num_of_chunks;

  /* CALCULATE SIZE OF EACH CHUNK */
  /* NB! CURRENTLY ONLY LAST CHUNK WILL DIFFER IN SIZE */
  size_t * chunk_sizes = malloc(num_of_chunks * sizeof(size_t));
  for (i = 0; i < num_of_chunks -1; chunk_sizes[i++] = chunk_size);
  chunk_sizes[num_of_chunks - 1] = target_size - chunk_size * (num_of_chunks - 1);

  /* CALCULATE OFFSET FOR EACH CHUNK */
  size_t * chunk_offset = malloc(num_of_chunks * sizeof(size_t));
  for(i = 0; i < num_of_chunks; i++) {
    chunk_offset[i] = i * chunk_size;
  }

  /* Initialize map keeping track transducer states. */
  init_outputmap();

  /* Array of linked lists, each containing jobs for a given chunk */
  joblist = calloc(num_of_chunks * sizeof(job_t*), 1);
  /* Array holding transducer states needed to generate the output */
  path = calloc(num_of_chunks * sizeof(job_t*), 1);

  /* Instantiates a thread pool */
  // TODO: Add argument for minimum number of threads, and for linger time
  pool = thr_pool_create(nthreads / 2, nthreads, 1, NULL);

  /* Create job for first data chunk */
  init_task = malloc(sizeof(job_t));

  init_task->cancled = 0;
  init_task->completed = 0;
  init_task->start_state = -1;
  init_task->phase = 0;
  init_task->chunk = 0;
  init_task->tstate = init(target, chunk_sizes[0], false, false);
  init_task->tstate->curr_state = -1;

  enqueue_job(init_task);

  /* Add transducer state to output map and path */
  outputmap[0][0][0] = path[0] = init_task->tstate;

  /* Add 'job generation' jobs for each chunk */
  for (i = 1; i  < num_of_chunks; i++) {
    task_gen_arg_t * arg = malloc(sizeof(task_gen_arg_t));
    arg->chunk = i;
    arg->offset = chunk_offset[i];
    arg->phase = 0;
    arg->suffix_length = suffix_len;
    arg->chunk_lengt = chunk_sizes[i];

    thr_pool_queue_prioritized(pool, generate_tasks, arg);
  }

  /* Wait for jobs to finish */
  // TODO: Destroy pool if we reach fail state
  thr_pool_wait(pool);

  if (pool)
  {
    thr_pool_destroy(&pool);
  }

  /* Clean up */
  free(chunk_sizes);
  free(chunk_offset);
}

/**
 *    Handles single chunk by processing entire file in one call to match.
 */
void run_single_chunk() {
  transducer_state * tstate = init(target, target_size, false, true);
  tstate->curr_state = -1;
  path = malloc(sizeof(transducer_state*));
  path[0] = tstate;
  match(1, tstate, NULL, true);
}

static struct option long_options[] = {
  { "chunks"      , required_argument, 0, 'c' },
  { "file"        , required_argument, 0, 'f' },
  { "help"        , no_argument      , 0, 'h' },
  { "len-suffix"  , required_argument, 0, 'l' },
  { "threads"     , required_argument, 0, 'p' },
  { "time"        , no_argument      , 0, 't' },
  { "time-all"    , no_argument      , 0, TIME_ALL    },
  { "time-data"   , no_argument      , 0, TIME_DATA   },
  { "time-fst"    , no_argument      , 0, TIME_FST    },
  { "time-output" , no_argument      , 0, TIME_OUTPUT },
  { 0, 0, 0, 0 }
};


int main(int argc, char *argv[]) {
  int c;
  int time_flags = 0;
  int option_index = 0;
  bool chunks_provided = false;
  bool nthreads_provided = false;
  bool do_timing = false;
  int suffix_len = DEFAULT_SUFFIX_LENGTH;
  num_of_chunks = DEFAULT_NUMBER_OF_CHUNKS;
  int nthreads = DEFAULT_NUMBER_OF_CHUNKS;
  char* file_name = NULL;

  if (argc == 1) {
    print_usage(argv[0]);
    return RETC_PRINT_USAGE;
  }

  while ((c = getopt_long(argc, argv, ":htl:f:c:p:", long_options, &option_index)) != -1) {
    switch (c)
    {
      case 'f':
        /* handle -f and --file */
        file_name = optarg;
        break;

      case 'c':
        /* handle -c and --chunks */
        num_of_chunks = atoi(optarg);
        chunks_provided = 1;
        if (num_of_chunks == 1 && !nthreads_provided) {
          nthreads = 1;
        }
        break;

      case 'p':
        /* handle -p and --threads */
        nthreads = atoi(optarg);
        if (num_of_chunks < 1) {
          fprintf(stderr,
                  "%s: arguments '%s' is invalid for option [-p | --threads]\n"
                  "  argiment must be a positive integer\n\n",
                  argv[0], optarg);
          print_usage(argv[0]);
          return RETC_PRINT_ERROR;
        }
        break;

      case 'l':
        /* handle -l and --len-suffix */
        suffix_len = atoi(optarg);
        if (suffix_len <= 0) {
          fprintf(stderr,
                  "%s: argument '%s' is invalid for option [-l | --len-suffix]\n"
                  "  argument must be a positive integer\n\n",
                  argv[0], optarg);
          print_usage(argv[0]);
          return RETC_PRINT_ERROR;
        }
        break;

      case TIME_ALL:
      case TIME_DATA:
      case TIME_FST:
      case TIME_OUTPUT:
        /* handle --time-all, --time-data, --timeprocess and --time-output */
        do_timing = 1;
        time_flags |= c;
        break;

      case 't':
        /* handle -t and --time */
        do_timing = 1;
        time_flags |= 0xff;
        break;

      case ':':
        /* missing option argument */
        fprintf(stderr, "%s: option '-%c' requires an argument\n\n",
                argv[0], optopt);
        print_usage(argv[0]);
        return RETC_PRINT_ERROR;
        break;

      case '?':
        /* invalid option */
        fprintf(stderr, "%s: option '-%c' is invalid\n\n",
                argv[0], optopt);
        print_usage(argv[0]);
        return RETC_PRINT_ERROR;

      case 'h':
      default:
        /* handle -h and --help*/
        print_usage(argv[0]);
        return RETC_PRINT_USAGE;
    }
  }

  /* Validate provided arguments */
  /* Check that file_name was provided */
  if (file_name == NULL) {
    fprintf(stderr, "%s: option ['-f' | '--file'] is required\n\n",
            argv[0]);
    print_usage(argv[0]);
    return RETC_PRINT_ERROR;
  }

  /* Check that file can be accessed */
  if (access(file_name, R_OK)) {
    switch (errno) {
      case ENOENT:
        /* file does not exist*/
        fprintf(stderr, "%s: the file '%s' does not exist\n", argv[0], file_name);
        return RETC_PRINT_ERROR;
        break;

      default:
        fprintf(stdout, "Error no.: %d", errno);
        break;
    }
  }

  /* Validate number of chunks */
  if (!chunks_provided) {
    fprintf(stderr, "%s: option ['-c' | '--chunks'] is required\n\n",
            argv[0]);
    print_usage(argv[0]);
    return RETC_PRINT_ERROR;
  } else if (num_of_chunks < 1) {
    fprintf(stderr, "%s: argument for option ['-c' | '--chunks'] must be > 0\n\n",
            argv[0]);
    print_usage(argv[0]);
    return RETC_PRINT_ERROR;
  }

  if (nthreads == 1 && num_of_chunks > 1) {
    fprintf(stderr,
            "%s: invalid combination of [-c | --chunks] and [-p | --threads]\n"
            "  Single thread with multiple chunks, setting number of chunks to 1\n"
            ,argv[0]);
    num_of_chunks = 1;
  } else if (nthreads > 1 && num_of_chunks == 1) {
    fprintf(stderr,
            "%s: invalid combination of [-c | --chunks] and [-p | --threads]\n"
            "  Multiple threads with single chunk, setting number of threads to 1\n",
            argv[0]);
    nthreads = 1;
  }

  if (do_timing) {
    gettimeofday(&time_start, NULL);
  }

  init_state_table();
  init_target(file_name);


  if (do_timing) {
    gettimeofday(&time_data, NULL);
  }


  /* Handle single chunk */
  if (num_of_chunks == 1) {
    run_single_chunk();
  } else {
    /* Handle multiple chunks */
    run_multi_chunk(suffix_len, nthreads);
  }

  if (do_timing) {
    gettimeofday(&time_fst, NULL);
  }

  output(path[0]);
  if (do_timing) {
    gettimeofday(&time_output, NULL);
  }

  if (do_timing) {
    print_timeing(time_flags);
  }
  return 0;
}

void print_timeing(int flags)
{
  struct timeval time_result;
  long int millis;

  if (flags & TIME_ALL)
  {
    /* Output total time */
    timersub(&time_output, &time_start, &time_result);
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "total time (ms): %ld\n", millis);
  }

  if (flags & TIME_DATA)
  {
    /* Output time for mapping data to memory */
    timersub(&time_data, &time_start, &time_result);
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "data time (ms): %ld\n", millis);
  }

  if (flags & TIME_FST)
  {
    /* Output time for processing data */
    timersub(&time_fst, &time_data, &time_result);
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "FST time (ms): %ld\n", millis);
  }

  if (flags & TIME_OUTPUT)
  {
    /* Output time for outputting data */
    timersub(&time_output, &time_fst, &time_result);
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "output time (ms): %ld\n", millis);
  }
}


/**
 *    Print usage text to stdout.
 */
void print_usage(char *name)
{
  fprintf(stdout,
          "usage: %s --file <file> --chunks <num>\n\n"
          "Help Options:\n"
          "  -h, --help\n"
          "    Displays this help text.\n"
          "\n"
          "Required Arguments:\n"
          "  -f, --file :: string\n"
          "    Specifies the path of the target file.\n"
          "  -c, --chunks :: int\n"
          "    Specifies number of threads to utilize.\n"
          "\n"
          "Optional Arguments\n"
          "  -l, --len-suffix :: int\n"
          "    Specifies the length of the suffixes used for suffix analysis.\n"
          "    default: 256\n"
          "  -p, --threads :: int\n"
          "    Specifies the number threads to populate the thread pool with.\n"
          "    default: # of online cores on system\n"
          "  -t, --time\n"
          "    Times execution with all timing flags set.\n"
          "  --time-all\n"
          "    Sets flag for timing entire execution.\n"
          "  --time-fst\n"
          "    Sets flag for timing data processing.\n"
          "  --time-data\n"
          "    Sets flag for timing reading data to memory.\n"
          "  --time-output\n"
          "    Sets flag for timing writing output data to stdout,\n",
          name);
}
