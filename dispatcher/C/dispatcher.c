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
#include <sys/types.h>  //  To acces waitpid(2)
#include <sys/wait.h>   //  --||--
#include <unistd.h>
#include <string.h>

#include <omp.h>

#include "email.h"
#include "util.h"
#include "list.h"

#define RETC_PRINT_USAGE     1
#define RETC_PRINT_ERROR     2
#define DEFAULT_SUFFIX_LENGTH 256

typedef struct job_node {
  int chunk;
  int state;
  int started;
} job_t;

char* target;
long target_size;


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
          "    default: 256\n",
          name);
}


/**
 *    Helper function for allocating shared memory.
 */
void* ammap(size_t len) {
  return mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_SHARED, -1, 0);
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

int did_accept(int state) {
  if (0 <= state && state < state_count ) {
    return state_table[state].accepting;
  }
  
  fprintf(stderr, "State %i does not exist.", state);
  exit(EXIT_FAILURE);
}


/**
 *    Pretty print job_node struct
 */
void print_job(void* job) {
  fprintf(stdout, "\033[1mjob\033[0m - c %i, s %i, b %i\n",
          ((struct job_node*)job)->chunk,
          ((struct job_node*)job)->state,
          ((struct job_node*)job)->started);
}

/**
 *    (OBS!!!) Assuming that states are starts from 0 and are consequtive.
 *
 *    Performs suffix analysis of chunk starting at the specified offset.
 *
 *    Returns an array mapping start state (index) to a final state (value),
 *    for the specified suffix.
 *
 *    int offest:     offset in target where the chunk begins.
 *    int suffix_len: length of suffix.
 */
int* suffix_analysis(long offset, long suffix_len) {
  int i;
  char* suffix = target + offset - suffix_len;
  int* map = malloc(state_count * sizeof(int));
  
#pragma omp parallel for
  for (i = 0; i < state_count; i++) {
    map[i] = match(1, i, suffix, suffix_len);
  }
  
  return map;
}


/**
 *    Handles single chunk by processing entire file in one call to match.
 */
void run_single_chunk() {
  int result;
  
  result = match(1, -1, target, target_size);
  fprintf(stdout, "-p 1 -s %i\n", result);
  if(did_accept(result)) {
    fprintf(stdout,"\033[1mDID ACCEPT\033[0m\n");
  } else {
    fprintf(stdout, "\033[1mDID NOT ACCEPT\033[0m\n");
  }
}


/**
 *    Remove removed outdated jobs based on content of result map.
 *    The resulting list contains remaining valid jobs.
 */
void update_jobs(int** resultmap, int num_of_chunks, struct node ** job_list) {
  int chunk = 0;
  int idx = 0;
  while (chunk < num_of_chunks && resultmap[chunk][idx] != -2) {
    idx = resultmap[chunk][idx];
    chunk++;
  }
  
  while (*job_list != NULL) {
    job_t * job = (*job_list)->data;
    if (job->chunk <= chunk) {
      pop(job_list);
    } else {
      break;
    }
  }
  
  if (chunk >= num_of_chunks) return;
  
  job_t * job = malloc(sizeof(job_t));
  job->chunk = chunk;
  job->state = idx;
  job->started = 0;
  
  push(job_list, job, sizeof(job_t));
  free(job);
}


int map_output(int** resultmap, int num_of_chunks) {
  int i, idx = 0;
  for (i = 0; i < num_of_chunks; i++) {
    idx = resultmap[i][idx];
  }
  return idx;
}


void run_multi_chunk(int num_of_chunks, int suffix_len) {
  int i, j;
  
  /* SET UP */
  long chunk_size = target_size / num_of_chunks;
  
  long * chunk_sizes = malloc(num_of_chunks * chunk_size);
  for (i = 0; i < num_of_chunks -1; chunk_sizes[i++] = chunk_size);
  chunk_sizes[num_of_chunks - 1] = target_size - chunk_size * (num_of_chunks - 1);
  
  /* CALCULATE OFFSET FOR EACH CHUNK */
  long * chunk_offset = malloc(num_of_chunks * sizeof(chunk_offset));
  for(i = 0; i < num_of_chunks; i++) {
    chunk_offset[i] = i * chunk_size;
  }
  
  /* DO SUFFIX ANALYSIS */
  int ** chunkmap = malloc(num_of_chunks * sizeof(chunkmap));
  #pragma omp parallel for
  for (i = 1; i < num_of_chunks; i++) {
    chunkmap[i] = suffix_analysis(chunk_offset[i], suffix_len);
  }
  
  // RUN KLEENEX
  int ** resultmap = malloc(num_of_chunks * sizeof(resultmap)); // Map containing result of each chunk
#pragma omp parallel for
  for (i = 0; i < num_of_chunks; i++) {                     // Initialize resultmap
    int * temp = malloc(state_count * sizeof(temp));
    for (j = 0; j < state_count; temp[j++] = -2);
    resultmap[i] = temp;
  }
  
  
  // CREATE JOBS
  size_t job_size = sizeof(job_t);
  
  struct node * job_list = NULL;
  struct job_node * job = malloc(job_size);
  
  for (i = num_of_chunks - 1; i > 0; i--) { // start at 1 as we know the start state of chunk 0.
    int * ss = unique(chunkmap[i], state_count);
    j = 0;
    /* Can we do this in a neater way? */
    while (ss[j] > -2) {
      job = malloc(job_size);
      job->chunk = i; job->state = ss[j]; job-> started = 0;
      push(&job_list, job, job_size);
      j++;
    }
  }
  
  /* Push job for chunk 0 */
  job->chunk = 0; job->state = -1; job-> started = 0;
  push(&job_list, job, job_size);
  
  printList(job_list, &print_job);
  
  while (job_list != NULL) {
#pragma omp parallel
    {
      struct job_node * job;
      long offset;
      int start_state, chunk;
#pragma omp critical
      {
        /* GET NEXT JOB */
        /* THIS SHOULD BE DONE IN ANOTHER WAY */
        if (job_list != NULL) {
          job = (struct job_node *)job_list->data;
          chunk = job->chunk;
          start_state = job->state;
          pop(&job_list);
        } else {
          chunk = -2;
        }
      }
      if (chunk != -2) {
        
        offset = chunk * chunk_size;
        
        if (chunk == 0) {
          resultmap[chunk][0] = match(1, start_state, target, chunk_sizes[chunk]);
        } else {
          resultmap[chunk][start_state] = match(1, start_state, target + chunk_offset[chunk], chunk_sizes[chunk]);
        }
      }
    }
    /* REMOVE OUTDATED JOBS */
    update_jobs(resultmap, num_of_chunks, &job_list);
  }
  
  fprintf(stdout, "printing results:\n");
  for (i = 0; i < num_of_chunks; i++) {
    fprintf(stdout, "chunk %i: ", i);
    for (j = 0; j < state_count; j++) {
      fprintf(stdout, "%i, ", resultmap[i][j]);
    }
    fprintf(stdout, "\n");
  }
  
  int final_state = map_output(resultmap, num_of_chunks);
  
  fprintf(stdout, "-p 1 -s %i\n", final_state);
  if(did_accept(final_state)) {
    fprintf(stdout,"\033[1mDID ACCEPT\033[0m\n");
  } else {
    fprintf(stdout, "\033[1mDID NOT ACCEPT\033[0m\n");
  }
}

static struct option long_options[] = {
  { "chunks"      , required_argument, 0, 'c' },
  { "file"        , required_argument, 0, 'f' },
  { "help"        , no_argument      , 0, 'h' },
  { "len-suffix"  , required_argument, 0, 'l' },
  { 0, 0, 0, 0 }
};


int main(int argc, char *argv[]) {
  int c;
  int option_index = 0;
  int num_of_chunks = 0;
  int chunks_provided = 0;
  int suffix_len = DEFAULT_SUFFIX_LENGTH;
  char* file_name = NULL;
  
  if (argc == 1) {
    print_usage(argv[0]);
    return RETC_PRINT_USAGE;
  }
  
  while ((c = getopt_long(argc, argv, ":hl:f:c:", long_options, &option_index)) != -1) {
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
  
  /* Handle multiple chunks */
  init_target(file_name);
  
  /* Handle single chunk */
  if (num_of_chunks == 1) {
    run_single_chunk();
    return 0;
  }
  
  run_multi_chunk(num_of_chunks, suffix_len);
  
  return 0;
}
