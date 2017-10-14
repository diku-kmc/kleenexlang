#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/time.h>
#include <stdio.h>
#ifdef __APPLE__
#   include <mach-o/dyld.h>
#endif

#define RETC_PRINT_USAGE     1
#define RETC_PRINT_INFO      2
#define RETC_PRINT_TABLE     3

#define OUTBUFFER_SIZE       (16*1024)
#define INBUFFER_SIZE        (16*1024)
#define INITIAL_BUFFER_SIZE  (4096*8)
#define OUTBUFFER_STACK_SIZE (1024)
#define avail (in_size - in_cursor)


#ifdef FLAG_NOINLINE
#define INLINE static
#endif
#ifndef INLINE
#define INLINE static inline
#endif

#ifndef BUFFER_UNIT_T
#warning "BUFFER_UNIT_T not defined. Falling back to default 'uint8_t'"
#define BUFFER_UNIT_T uint8_t
#endif

#ifndef NUM_PHASES
#warning "NUM_PHASES not defined."
#endif

#ifndef OUTSTREAM
#define OUTSTREAM stdout
#endif

// Order of descriptors provided by pipe()
#define  READ_FD 0
#define WRITE_FD 1

typedef struct symbol {
  size_t position;
  int reg;
  struct symbol * next;
} symbol;

typedef BUFFER_UNIT_T buffer_unit_t;
typedef struct {
  buffer_unit_t *data;
  size_t size;         /* size in bytes */
  size_t bitpos;       /* bit offset from data  */
  symbol * symbols;
} buffer_t;

typedef struct {
  buffer_unit_t *data;
  size_t cursor;
  size_t size;
  long length;    // static length of input, -1 if unknown
  buffer_unit_t *next; /* pointer to next data */
  bool clear_data_on_delete;
} input_buffer;

#define BUFFER_UNIT_SIZE (sizeof(buffer_unit_t))
#define BUFFER_UNIT_BITS (BUFFER_UNIT_SIZE * 8)

typedef struct t_state {
  buffer_t **buffers;
  buffer_t *outbuf;
  size_t output_cursor;
  input_buffer *inbuf;
  int curr_state;
  struct t_state ** nextPtr;
  struct t_state * src;
} transducer_state;

// State information
typedef struct {
  int num;
  int accepting;
} state;

// Program interface

extern int state_count[];
extern state ** state_table;
extern int phases;
void init_state_table();

void printCompilationInfo();

/*
 *    Initializes a tranducer state.
 *
 *    unsigned char* input: input for SST to process
 *    size_t input_size:    size of input
 *    int add_symbols:      specifies is symbols representing
 */
transducer_state *init(unsigned char* input, size_t input_size, bool copy_data, bool add_symbols);

/*
 *    Frees a transducer state and its content.
 */
void free_state(transducer_state *);

void match(int phase, transducer_state *state, void (*callback)(transducer_state*), bool is_final);

/* 
 * Pure transition function, no output generated
 */
int silent_match(int phase, int start_state, unsigned char * buf, long length);

