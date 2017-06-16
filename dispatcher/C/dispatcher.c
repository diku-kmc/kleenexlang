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
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

#include "thr_pool.h"
#include "crt.h"

//#define DEBUGGING
#define RETC_PRINT_USAGE      1
#define RETC_PRINT_ERROR      2
#define DEFAULT_SUFFIX_LENGTH 256

typedef struct job_t {
  struct job_t *next;
  int start_state;
  int chunk;
  int phase;
  int completed;
  int cancled;
  job_id job;
  transducer_state *tstate;
} job_t;

typedef struct {
  int phase;
  int chunk;
  size_t offset;
  size_t chunk_lengt;
  size_t suffix_length;
  thr_pool_t * pool;
} task_gen_arg_t;

/* Mutex */
static pthread_mutex_t job_list_lock = PTHREAD_MUTEX_INITIALIZER; /* Protects job list */
static pthread_mutex_t path_lock = PTHREAD_MUTEX_INITIALIZER; /* Protects job list */


unsigned char* target;
long target_size;

int num_of_chunks;

transducer_state **** outputmap;
thr_pool_t *pool;


/* job list keeping track of active jobs */
job_t **joblist;

transducer_state **path;

/* Function declarations */
void print_usage(char *);
void enqueue_job(job_t *);

/* thread jobs */
void *calculate(void *);
void *generate_tasks(void *);
void *update_path_and_clean(void *);

void *update_path_and_clean(void *arg)
{
  job_t *job = (job_t *)arg;
  job_t *head = NULL, *next = NULL;

  (void) pthread_mutex_lock(&job_list_lock);

  /* Check that job has finished */
  if (! job->completed) {
    (void) pthread_mutex_unlock(&job_list_lock);
    return (0);
  }

  (void) pthread_mutex_lock(&path_lock);

  /* If job is not on path stop */
  if (path[job->chunk] != job->tstate){
    (void) pthread_mutex_unlock(&path_lock);
    (void) pthread_mutex_unlock(&job_list_lock);
    return (0);
  }

  for (head = joblist[job->chunk];
       head != NULL;)
  {
    next = head->next;
    if (head->job != job->job && !job->completed)
    {
      thr_pool_dequeue(pool, job->job);
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
  (void) pthread_mutex_unlock(&job_list_lock);

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

  /* Handle fail state */
  if (tstate->curr_state == -1) {
    for (chunk = 0; chunk < num_of_chunks && path[chunk] != tstate && path[chunk] != NULL; chunk++);
    position = (target_size / num_of_chunks) * chunk + tstate->inbuf->cursor;
    fprintf(stderr, "Match error at input symbol %zu, (next char: %u)!\n", position, tstate->inbuf->next[0]);
    return;
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
      task->cancled = 0;
      task->completed = 0;
      task->chunk = chunk;
      task->next = NULL;
      task->phase = phase;
      task->tstate = init(start, length, 1);
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
 *    Function for processing a chunk of data.
 */
void *calculate(void *arg)
{
  int old_state;
  job_t * task = (job_t *)arg;
  int phase, chunk;
  transducer_state *tstate;

  if (task->completed) return (0);

  if (task->cancled)
  {
    task = NULL;
    return (0);
  }

  phase = task->phase;
  chunk = task->chunk;
  tstate = task->tstate;

  match(phase+1, tstate, NULL);

  if (chunk < num_of_chunks-1)
  {
    tstate->nextPtr = &outputmap[phase][chunk+1][tstate->curr_state];
  }

  task->completed = 1;
  /* Prevent thread from beeing canceled while holding a lock */
  /* NB! The thread might still terminate due other causes */
  (void) pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old_state);

  (void) pthread_mutex_lock(&path_lock);
  if (path[chunk] == tstate && tstate->nextPtr != NULL)
  {
    path[chunk+1] = *(tstate->nextPtr);
  }
  (void) pthread_mutex_unlock(&path_lock);

  thr_pool_queue_prioritized(pool, update_path_and_clean, task);

  /* Reset teh cancel state of the thread */
  (void) pthread_setcancelstate(old_state, NULL);

#ifdef DEBUGGING
  fprintf(stdout, "Job: phase: %i, chunk: %i, ended at state: %i\n", phase, chunk, tstate->curr_state);
#endif

  // SIGNAL THAT WE CAN COMBINE OUTPUT
  task = NULL;

  return (0);
}


/*
 *    Adds a job to the pool queue and registers it in the joblist
 */
void enqueue_job(job_t *task)
{
  (void) pthread_mutex_lock(&job_list_lock);

  task->next = joblist[task->chunk];
  joblist[task->chunk] = task;
  thr_pool_queue(pool, calculate, task, &task->job);

  (void) pthread_mutex_unlock(&job_list_lock);
}

void run_multi_chunk(int suffix_len) {
  int i;
  job_t *init_task;
  /* SET UP */
  long chunk_size = target_size / num_of_chunks;

  size_t * chunk_sizes = malloc(num_of_chunks * sizeof(size_t));
  for (i = 0; i < num_of_chunks -1; chunk_sizes[i++] = chunk_size);
  chunk_sizes[num_of_chunks - 1] = target_size - chunk_size * (num_of_chunks - 1);

  /* CALCULATE OFFSET FOR EACH CHUNK */
  size_t * chunk_offset = malloc(num_of_chunks * sizeof(size_t));
  for(i = 0; i < num_of_chunks; i++) {
    chunk_offset[i] = i * chunk_size;
  }

  init_outputmap();
  joblist = calloc(num_of_chunks * sizeof(job_t*), 1);
  path = calloc(num_of_chunks * sizeof(job_t*), 1);

  pool = thr_pool_create(num_of_chunks, num_of_chunks, 10, NULL);

  init_task = malloc(sizeof(job_t));

  init_task->cancled = 0;
  init_task->completed = 0;
  init_task->start_state = -1;
  init_task->phase = 0;
  init_task->chunk = 0;
  init_task->tstate = init(target, chunk_sizes[0], 0);
  init_task->tstate->curr_state = -1;

  outputmap[0][0][0] = path[0] = init_task->tstate;

  enqueue_job(init_task);

  for (i = 1; i  < num_of_chunks; i++) {
    task_gen_arg_t * arg = malloc(sizeof(task_gen_arg_t));
    arg->chunk = i;
    arg->offset = chunk_offset[i];
    arg->phase = 0;
    arg->suffix_length = suffix_len;
    arg->pool = pool;
    arg->chunk_lengt = chunk_sizes[i];

    thr_pool_queue_prioritized(pool, generate_tasks, arg);
  }

  thr_pool_wait(pool);

  if (pool)
  {
    thr_pool_destroy(&pool);
  }

  output(outputmap[0][0][0]);

  /* Clean up */
  free(chunk_sizes);
  free(chunk_offset);
}

/**
 *    Handles single chunk by processing entire file in one call to match.
 */
void run_single_chunk() {
  transducer_state * state = init(target, target_size, 0);
  state->curr_state = -1;
  match(1, state, NULL);
  fprintf(stdout, "%s", state->outbuf->data);
  // TODO: print error text if we did not reach a accept state;
}

static struct option long_options[] = {
  { "chunks"      , required_argument, 0, 'c' },
  { "file"        , required_argument, 0, 'f' },
  { "help"        , no_argument      , 0, 'h' },
  { "len-suffix"  , required_argument, 0, 'l' },
  { "time"        , no_argument      , 0, 't' },
  { 0, 0, 0, 0 }
};


int main(int argc, char *argv[]) {
  int c;
  int option_index = 0;
  int chunks_provided = 0;
  int do_timing = 0;
  int suffix_len = DEFAULT_SUFFIX_LENGTH;
  char* file_name = NULL;

  if (argc == 1) {
    print_usage(argv[0]);
    return RETC_PRINT_USAGE;
  }

  while ((c = getopt_long(argc, argv, ":htl:f:c:", long_options, &option_index)) != -1) {
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
        break;

      case 'l':
        /* handle -l and --len-suffix */
        suffix_len = atoi(optarg);
        if (suffix_len <= 0) {
          fprintf(stdout,
                  "%s: argument '%s' is invalid for option [-l | --len-suffix]\n"
                  "  argument must be a positive integer\n\n",
                  argv[0], optarg);
          print_usage(argv[0]);
          return RETC_PRINT_ERROR;
        }
        break;

      case 't':
        /* handle -t and --time */
        do_timing = 1;
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
    fprintf(stderr, "%s: option ['-c' | '--chunks'] is required\n\n",
            argv[0]);
    print_usage(argv[0]);
    return RETC_PRINT_ERROR;
  } else if (num_of_chunks < 1) {
    fprintf(stderr, "%s: argument for option ['-c' | '--chunks'] must be > 0\n\n",
            argv[0]);
    print_usage(argv[0]);
    return RETC_PRINT_ERROR;
  }

  struct timeval time_before, time_after, time_result;
  long int millis;
  if (do_timing) {
    gettimeofday(&time_before, NULL);
  }

  /* Handle multiple chunks */
  init_target(file_name);

  /* Handle single chunk */
  if (num_of_chunks == 1) {
    run_single_chunk();
    if (do_timing) {
      gettimeofday(&time_after, NULL);
      timersub(&time_after, &time_before, &time_result);
      // A timeval contains seconds and microseconds.
      millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
      fprintf(stderr, "time (ms): %ld\n", millis);
    }
    return 0;
  }

  run_multi_chunk(suffix_len);

  if (do_timing) {
    gettimeofday(&time_after, NULL);
    timersub(&time_after, &time_before, &time_result);
    // A timeval contains seconds and microseconds.
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "time (ms): %ld\n", millis);
  }
  return 0;
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
          "  -t, --time\n"
          "    Times execution and writes time to stderr\n",
          name);
}
