
#define NUM_PHASES 1
#define BUFFER_UNIT_T uint8_t
#include "crt.h"
state ** state_table;

void buf_flush(buffer_t *buf)
{
  size_t word_index = buf->bitpos / BUFFER_UNIT_BITS;
  // If we do not have a single complete word to flush, return.
  // Not just an optimization! The zeroing logic below assumes word_index > 0.
  if (word_index == 0)
  {
    return;
  }
  if (fwrite(buf->data, BUFFER_UNIT_SIZE, word_index, OUTSTREAM) == -1)
  {
    fprintf(stderr, "Error writing to output stream.\n");
    exit(1);
  }
  // Since partially written words are not flushed, they need to be moved to the
  // beginning of the buffer.
  if (buf->bitpos % BUFFER_UNIT_BITS != 0)
  {
    buf->data[0] = buf->data[word_index];
  }
  else
  {
    // If we flushed everything, re-establish the invariant that the word at the
    // cursor is garbage-free by simply zeroing it.
    buf->data[0] = 0;
  }

  // Rewind cursor
  buf->bitpos = buf->bitpos - word_index * BUFFER_UNIT_BITS;
}

// Write first 'bits' of 'w' to 'buf', starting from the MOST significant bit.
// Precondition: Remaining bits of 'w' must be zero.
INLINE
bool buf_writeconst(buffer_t *buf, buffer_unit_t w, size_t bits)
{
  size_t word_index = buf->bitpos / BUFFER_UNIT_BITS;
  size_t offset = buf->bitpos % BUFFER_UNIT_BITS;
  size_t bits_available = BUFFER_UNIT_BITS - offset;

#ifdef FLAG_WORDALIGNED
  buf->data[word_index] = w;
#else
  buf->data[word_index] |= w >> offset;
  // test for offset > 0 important; shifting by the word size is undefined behaviour.
  buf->data[word_index+1] = (offset == 0) ? 0 : (w << bits_available);
#endif

  buf->bitpos += bits;

  // Is cursor in last word?
  return (buf->bitpos >= buf->size * 8 - BUFFER_UNIT_BITS);
}

void buf_resize(buffer_t *buf, size_t shift)
{
  size_t new_size = buf->size << shift;
  buffer_unit_t *data2 = calloc(new_size, 1);
  memcpy(data2, buf->data, buf->size);
  free(buf->data);
  buf->data = data2;
  buf->size = new_size;
}

INLINE
void buf_writearray(buffer_t *dst, const buffer_unit_t *arr, size_t bits)
{
  if (dst->bitpos % BUFFER_UNIT_BITS == 0)
  {
    int count = (bits / BUFFER_UNIT_BITS) + (bits % BUFFER_UNIT_BITS ? 1 : 0);
    memcpy(&dst->data[dst->bitpos / BUFFER_UNIT_BITS], arr, count * BUFFER_UNIT_SIZE);
    dst->bitpos += bits;
    dst->data[dst->bitpos / BUFFER_UNIT_BITS] = 0;
  } else
  {
    int word_index = 0;
    for (word_index = 0; word_index < bits / BUFFER_UNIT_BITS; word_index++)
    {
      buf_writeconst(dst, arr[word_index], BUFFER_UNIT_BITS);
    }

    if (bits % BUFFER_UNIT_BITS != 0)
    {
      buf_writeconst(dst, arr[word_index], bits % BUFFER_UNIT_BITS);
    }
  }
}

INLINE
void reset(buffer_t *buf)
{
  buf->data[0] = 0;
  buf->bitpos = 0;
  buf->symbols = NULL;
}

buffer_t* init_buffer(size_t size)
{
  buffer_t *buf = malloc(sizeof(buffer_t));
  buf->data = malloc(size);
  buf->size = size;
  buf->bitpos = 0;
  buf->data[0] = 0;
  return buf;
}

input_buffer* init_input_buffer_by_ref(unsigned char* input_ref, size_t size)
{
  input_buffer *inbuf = malloc(sizeof(input_buffer));
  inbuf->data = input_ref;
  inbuf->size = size;
  inbuf->cursor = 0;
  inbuf->length = size;
  inbuf->next = inbuf->data;
  return inbuf;
}

input_buffer* init_input_buffer(size_t size)
{
  input_buffer *inbuf = malloc(sizeof(input_buffer));
  inbuf->data = malloc(size);
  inbuf->size = size;
  inbuf->cursor = 0;
  inbuf->length = -1;
  inbuf->next = inbuf->data;
  return inbuf;
}

void destroy_buffer(buffer_t *buf)
{
  if (buf->data != NULL)
    free(buf->data);
  buf->data = NULL;
}

INLINE
void appendarray(buffer_t *dst, const buffer_unit_t *arr, size_t bits)
{
  size_t total_bits = dst->bitpos + bits;
  if (total_bits >= (dst->size - 1) * BUFFER_UNIT_BITS * BUFFER_UNIT_SIZE)
  {
    size_t shift = 1;
    while (total_bits >= ((dst->size << shift) - 1) * BUFFER_UNIT_BITS * BUFFER_UNIT_SIZE)
    {
      shift++;
    }
    buf_resize(dst, shift);
  }

  buf_writearray(dst, arr, bits);
}

INLINE
void append(buffer_t *buf, buffer_unit_t w, size_t bits)
{
  if (buf_writeconst(buf, w, bits))
  {
    buf_resize(buf, 1);
  }
}

INLINE
void concat(buffer_t *dst, buffer_t *src)
{
  size_t old_pos = dst->bitpos;
  symbol *src_sym, *dst_sym;
  appendarray(dst, src->data, src->bitpos);

  if (src->symbols) {
    /* Get end of symbol list */
    for (src_sym = src->symbols;
         src_sym != NULL;
         src_sym = src_sym->next) {
      src_sym->position += old_pos;
    }
    if (dst->symbols) {
      /* Get end of symbol list */
      for (dst_sym = dst->symbols;
           dst_sym->next != NULL;
           dst_sym = dst_sym->next);

      dst_sym->next = src->symbols;
    }
    else
    {
      dst->symbols = src->symbols;
    }
  }
}

INLINE
void consume(transducer_state *tstate, int c)
{
  tstate->inbuf->next   += c;
  tstate->inbuf->cursor += c;
}

INLINE
int cmp(unsigned char *str1, unsigned char *str2, int l)
{
  int i = 0;
  for (i = 0; i < l; i++)
  {
    if (str1[i] != str2[i])
      return 0;
  }
  return 1;
}

/* no tables */

int state_count[1] = {12};
int phases = 1;
void init_state_table() {
  state_table = malloc(1 * sizeof(state*));
  state_table[0] = malloc(state_count[0] * sizeof(state));
  state_table[0][0] = ((state){.num =  0, .accepting = 0});
  state_table[0][1] = ((state){.num =  1, .accepting = 0});
  state_table[0][2] = ((state){.num =  2, .accepting = 0});
  state_table[0][3] = ((state){.num =  3, .accepting = 0});
  state_table[0][4] = ((state){.num =  4, .accepting = 1});
  state_table[0][5] = ((state){.num =  5, .accepting = 0});
  state_table[0][6] = ((state){.num =  6, .accepting = 0});
  state_table[0][7] = ((state){.num =  7, .accepting = 0});
  state_table[0][8] = ((state){.num =  8, .accepting = 0});
  state_table[0][9] = ((state){.num =  9, .accepting = 0});
  state_table[0][10] = ((state){.num =  10, .accepting = 0});
  state_table[0][11] = ((state){.num =  11, .accepting = 0});
}

// \xa
const buffer_unit_t const_1_0[1] = {0xa};
// -
const buffer_unit_t const_1_1[1] = {0x2d};
// .
const buffer_unit_t const_1_2[1] = {0x2e};
// @
const buffer_unit_t const_1_3[1] = {0x40};
void printCompilationInfo()
{
  fprintf(stdout, "No object file generated!\nOptions:\nSST optimization level:  3\nWord size:               UInt8T\nIdentity tables removed: False\n\nSource file: ../../bench/kleenex/src/email.kex\nSource md5:  65b11bd751a334edf99e8f11582189f9\nSST states:  12\n");
}

transducer_state* init(unsigned char* input, size_t input_size, bool copy_data, bool add_symbols)
{

  int i;
  transducer_state* tstate = malloc(sizeof(transducer_state));

  // Init regular buffers
  tstate->buffers = malloc(8 * sizeof(buffer_t *));
  for (i = 1; i < 8; i++) {
    tstate->buffers[i] = init_buffer(INITIAL_BUFFER_SIZE);
    tstate->buffers[i]->symbols = NULL;
    if (add_symbols) {
      symbol * sym = malloc(sizeof(symbol));
      sym->next = NULL;
      sym->position = 0;
      sym->reg = i;
      tstate->buffers[i]->symbols = sym;
    }
  }

  // Init in/out buffers

  if (input) {
    if (copy_data) {
      tstate->inbuf  = init_input_buffer(input_size);
      memcpy(tstate->inbuf->data, input, input_size);
      tstate->inbuf->length = input_size;
    } else {
      tstate->inbuf = init_input_buffer_by_ref(input, input_size);
    }
    tstate->inbuf->clear_data_on_delete = copy_data;

    tstate->outbuf = init_buffer(4096);
    tstate->buffers[0] = tstate->outbuf;
    tstate->buffers[0]->symbols = NULL;
    tstate->output_cursor = 0;
  }

  tstate->nextPtr = NULL;
  tstate->src = NULL;
  return tstate;

}

void free_state(transducer_state * tstate)
{

  int i;

  for (i = 0; i < 8; i++) {
    free(tstate->buffers[i]->data);
    tstate->buffers[i]->data = NULL;
    free(tstate->buffers[i]);
    tstate->buffers[i] = NULL;
  }

  if (tstate->inbuf->clear_data_on_delete) {
    tstate->inbuf->next = NULL;
    free(tstate->inbuf->data);
    free(tstate->inbuf);
    tstate->inbuf = NULL;
  } else {
    tstate->inbuf->next = NULL;
    tstate->inbuf->data = NULL;
    free(tstate->inbuf);
    tstate->inbuf = NULL;
  }

  tstate->outbuf = NULL;
  tstate->nextPtr = NULL;
  tstate = NULL;

}

void match1(transducer_state* tstate, void (*callback)(transducer_state*), bool is_final)
{
  long left = tstate->inbuf->length;
  if (tstate->curr_state < 0) goto l1_0;
  switch (tstate->curr_state) {
    case 0:
      goto l1_0;
    case 1:
      goto l1_1;
    case 2:
      goto l1_2;
    case 3:
      goto l1_3;
    case 4:
      goto l1_4;
    case 5:
      goto l1_5;
    case 6:
      goto l1_6;
    case 7:
      goto l1_7;
    case 8:
      goto l1_8;
    case 9:
      goto l1_9;
    case 10:
      goto l1_10;
    case 11:
      goto l1_11;
    default:
      tstate->curr_state = -1;
      return;
  }
  l1_0:
  if (left < 1)
  {
     tstate->curr_state = 0;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ' ')) || ((tstate->inbuf->next[0] == '"') || ((('(' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ')')) || ((tstate->inbuf->next[0] == ',') || ((tstate->inbuf->next[0] == '.') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '<')) || ((tstate->inbuf->next[0] == '>') || ((('@' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '[')) || ((tstate->inbuf->next[0] == ']') || ((127 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))))))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((tstate->inbuf->next[0] == '!') || ((('#' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 39)) || ((('*' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '+')) || ((tstate->inbuf->next[0] == '-') || ((('/' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || ((tstate->inbuf->next[0] == '=') || ((tstate->inbuf->next[0] == '?') || ((tstate->inbuf->next[0] == 92) || (('^' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '~')))))))))) && 1)))
  {
     reset(tstate->buffers[1]);
     append(tstate->buffers[1],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_5;
  }
  tstate->curr_state = -1;
  return;
  l1_1:
  if (left < 1)
  {
     tstate->curr_state = 1;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || ((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  tstate->curr_state = -1;
  return;
  l1_2:
  if (left < 1)
  {
     tstate->curr_state = 2;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ' ')) || ((tstate->inbuf->next[0] == '"') || ((('(' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ')')) || ((tstate->inbuf->next[0] == ',') || ((tstate->inbuf->next[0] == '.') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '<')) || ((tstate->inbuf->next[0] == '>') || ((('@' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '[')) || ((tstate->inbuf->next[0] == ']') || ((127 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))))))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((tstate->inbuf->next[0] == '!') || ((('#' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 39)) || ((('*' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '+')) || ((tstate->inbuf->next[0] == '-') || ((('/' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || ((tstate->inbuf->next[0] == '=') || ((tstate->inbuf->next[0] == '?') || ((tstate->inbuf->next[0] == 92) || (('^' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '~')))))))))) && 1)))
  {
     append(tstate->buffers[1],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_6;
  }
  tstate->curr_state = -1;
  return;
  l1_3:
  if (left < 1)
  {
     tstate->curr_state = 3;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '/')) || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '`')) || (('{' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((('0' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || (('a' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 'z'))) && 1)))
  {
     reset(tstate->buffers[5]);
     reset(tstate->buffers[2]);
     append(tstate->buffers[1],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_9;
  }
  tstate->curr_state = -1;
  return;
  l1_4:
  if (left < 1)
  {
     tstate->curr_state = 4;
     return; //accept
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ' ')) || ((tstate->inbuf->next[0] == '"') || ((('(' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ')')) || ((tstate->inbuf->next[0] == ',') || ((tstate->inbuf->next[0] == '.') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '<')) || ((tstate->inbuf->next[0] == '>') || ((('@' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '[')) || ((tstate->inbuf->next[0] == ']') || ((127 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))))))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((tstate->inbuf->next[0] == '!') || ((('#' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 39)) || ((('*' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '+')) || ((tstate->inbuf->next[0] == '-') || ((('/' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || ((tstate->inbuf->next[0] == '=') || ((tstate->inbuf->next[0] == '?') || ((tstate->inbuf->next[0] == 92) || (('^' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '~')))))))))) && 1)))
  {
     reset(tstate->buffers[1]);
     append(tstate->buffers[1],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_5;
  }
  tstate->curr_state = -1;
  return;
  l1_5:
  if (left < 1)
  {
     tstate->curr_state = 5;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ' ')) || ((tstate->inbuf->next[0] == '"') || ((('(' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ')')) || ((tstate->inbuf->next[0] == ',') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '<')) || ((tstate->inbuf->next[0] == '>') || ((('A' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '[')) || ((tstate->inbuf->next[0] == ']') || ((127 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255))))))))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((tstate->inbuf->next[0] == '!') || ((('#' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 39)) || ((('*' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '+')) || ((tstate->inbuf->next[0] == '-') || ((('/' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || ((tstate->inbuf->next[0] == '=') || ((tstate->inbuf->next[0] == '?') || ((tstate->inbuf->next[0] == 92) || (('^' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '~')))))))))) && 1)))
  {
     append(tstate->buffers[1],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_5;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '.') && 1)))
  {
     appendarray(tstate->buffers[1],const_1_2,8);
     consume(tstate,1);
     left -= 1;
     goto l1_2;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '@') && 1)))
  {
     appendarray(tstate->buffers[1],const_1_3,8);
     consume(tstate,1);
     left -= 1;
     goto l1_3;
  }
  tstate->curr_state = -1;
  return;
  l1_6:
  if (left < 1)
  {
     tstate->curr_state = 6;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ' ')) || ((tstate->inbuf->next[0] == '"') || ((('(' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ')')) || ((tstate->inbuf->next[0] == ',') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '<')) || ((tstate->inbuf->next[0] == '>') || ((('A' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '[')) || ((tstate->inbuf->next[0] == ']') || ((127 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255))))))))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((tstate->inbuf->next[0] == '!') || ((('#' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 39)) || ((('*' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '+')) || ((tstate->inbuf->next[0] == '-') || ((('/' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || ((tstate->inbuf->next[0] == '=') || ((tstate->inbuf->next[0] == '?') || ((tstate->inbuf->next[0] == 92) || (('^' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '~')))))))))) && 1)))
  {
     append(tstate->buffers[1],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_6;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '.') && 1)))
  {
     appendarray(tstate->buffers[1],const_1_2,8);
     consume(tstate,1);
     left -= 1;
     goto l1_2;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '@') && 1)))
  {
     appendarray(tstate->buffers[1],const_1_3,8);
     consume(tstate,1);
     left -= 1;
     goto l1_3;
  }
  tstate->curr_state = -1;
  return;
  l1_7:
  if (left < 1)
  {
     tstate->curr_state = 7;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '/')) || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '`')) || (('{' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((('0' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || (('a' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 'z'))) && 1)))
  {
     reset(tstate->buffers[7]);
     reset(tstate->buffers[6]);
     reset(tstate->buffers[5]);
     append(tstate->buffers[5],tstate->inbuf->next[0],8);
     reset(tstate->buffers[4]);
     reset(tstate->buffers[3]);
     reset(tstate->buffers[2]);
     append(tstate->buffers[2],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_11;
  }
  tstate->curr_state = -1;
  return;
  l1_8:
  if (left < 1)
  {
     tstate->curr_state = 8;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ',')) || ((('.' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '/')) || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '`')) || (('{' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '-') && 1)))
  {
     appendarray(tstate->buffers[1],const_1_1,8);
     consume(tstate,1);
     left -= 1;
     goto l1_8;
  }
  if (((left >= 1) && (((('0' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || (('a' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 'z'))) && 1)))
  {
     reset(tstate->buffers[5]);
     append(tstate->buffers[5],tstate->inbuf->next[0],8);
     reset(tstate->buffers[2]);
     append(tstate->buffers[2],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_9;
  }
  tstate->curr_state = -1;
  return;
  l1_9:
  if (left < 1)
  {
     tstate->curr_state = 9;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ',')) || ((tstate->inbuf->next[0] == '/') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '`')) || (('{' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '-') && 1)))
  {
     concat(tstate->buffers[1],tstate->buffers[2]);
     appendarray(tstate->buffers[1],const_1_1,8);
     consume(tstate,1);
     left -= 1;
     goto l1_8;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '.') && 1)))
  {
     concat(tstate->buffers[1],tstate->buffers[5]);
     appendarray(tstate->buffers[1],const_1_2,8);
     consume(tstate,1);
     left -= 1;
     goto l1_7;
  }
  if (((left >= 1) && (((('0' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || (('a' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 'z'))) && 1)))
  {
     reset(tstate->buffers[5]);
     append(tstate->buffers[5],tstate->inbuf->next[0],8);
     concat(tstate->buffers[1],tstate->buffers[2]);
     reset(tstate->buffers[2]);
     append(tstate->buffers[2],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_9;
  }
  tstate->curr_state = -1;
  return;
  l1_10:
  if (left < 1)
  {
     tstate->curr_state = 10;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ',')) || ((('.' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '/')) || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '`')) || (('{' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '-') && 1)))
  {
     appendarray(tstate->buffers[5],const_1_1,8);
     appendarray(tstate->buffers[2],const_1_1,8);
     consume(tstate,1);
     left -= 1;
     goto l1_10;
  }
  if (((left >= 1) && (((('0' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || (('a' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 'z'))) && 1)))
  {
     reset(tstate->buffers[7]);
     append(tstate->buffers[7],tstate->inbuf->next[0],8);
     reset(tstate->buffers[6]);
     append(tstate->buffers[6],tstate->inbuf->next[0],8);
     reset(tstate->buffers[4]);
     append(tstate->buffers[4],tstate->inbuf->next[0],8);
     reset(tstate->buffers[3]);
     append(tstate->buffers[3],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_11;
  }
  tstate->curr_state = -1;
  return;
  l1_11:
  if (left < 1)
  {
     tstate->curr_state = 11;
     return; //fail
  }
  if (((left >= 1) && ((((0 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 9)) || (((11 <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= ',')) || ((tstate->inbuf->next[0] == '/') || (((':' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '`')) || (('{' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 255)))))) && 1)))
  {
     consume(tstate,1);
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == 10) && 1)))
  {
     concat(tstate->buffers[0],tstate->buffers[1]);
     if (callback != NULL)
     {
       callback(tstate);
     }
     concat(tstate->buffers[0],tstate->buffers[5]);
     if (callback != NULL)
     {
       callback(tstate);
     }
     concat(tstate->buffers[0],tstate->buffers[7]);
     if (callback != NULL)
     {
       callback(tstate);
     }
     appendarray(tstate->buffers[0],const_1_0,8);
     if (callback != NULL)
     {
       callback(tstate);
     }
     consume(tstate,1);
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '-') && 1)))
  {
     concat(tstate->buffers[5],tstate->buffers[4]);
     appendarray(tstate->buffers[5],const_1_1,8);
     concat(tstate->buffers[2],tstate->buffers[3]);
     appendarray(tstate->buffers[2],const_1_1,8);
     consume(tstate,1);
     left -= 1;
     goto l1_10;
  }
  if (((left >= 1) && ((tstate->inbuf->next[0] == '.') && 1)))
  {
     concat(tstate->buffers[1],tstate->buffers[2]);
     concat(tstate->buffers[1],tstate->buffers[6]);
     appendarray(tstate->buffers[1],const_1_2,8);
     consume(tstate,1);
     left -= 1;
     goto l1_7;
  }
  if (((left >= 1) && (((('0' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= '9')) || (('a' <= tstate->inbuf->next[0]) && (tstate->inbuf->next[0] <= 'z'))) && 1)))
  {
     reset(tstate->buffers[7]);
     append(tstate->buffers[7],tstate->inbuf->next[0],8);
     reset(tstate->buffers[6]);
     append(tstate->buffers[6],tstate->inbuf->next[0],8);
     concat(tstate->buffers[5],tstate->buffers[4]);
     reset(tstate->buffers[4]);
     append(tstate->buffers[4],tstate->inbuf->next[0],8);
     concat(tstate->buffers[2],tstate->buffers[3]);
     reset(tstate->buffers[3]);
     append(tstate->buffers[3],tstate->inbuf->next[0],8);
     consume(tstate,1);
     left -= 1;
     goto l1_11;
  }
  tstate->curr_state = -1;
  return;
  //accept1:
  //  return;
  //fail1:
  //  fprintf(stderr, "Match error at input symbol %zu, (next char: %u)!\n", count, next[0]);
  //  exit(1);
}

void match(int phase, transducer_state* tstate, void (*callback)(transducer_state*), bool is_final)
{
  switch(phase) {
    case 1:
      return match1(tstate, callback, is_final);
      break;
    default:
      fprintf(stderr, "Invalid phase: %d given\n", phase);
      exit(1);
  }
}

int silent_match1(int start_state, unsigned char * buf, long length)
{
  long left = length;
  unsigned char * next = (unsigned char *)buf;
  if (start_state < 0) goto l1_0;
  switch (start_state) {
    case 0:
      goto l1_0;
    case 1:
      goto l1_1;
    case 2:
      goto l1_2;
    case 3:
      goto l1_3;
    case 4:
      goto l1_4;
    case 5:
      goto l1_5;
    case 6:
      goto l1_6;
    case 7:
      goto l1_7;
    case 8:
      goto l1_8;
    case 9:
      goto l1_9;
    case 10:
      goto l1_10;
    case 11:
      goto l1_11;
    default:
      return -1;
  }
  l1_0:
  if (left < 1)
  {
     return 0; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || ((next[0] == '.') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('@' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255)))))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_5;
  }
  return -1;
  l1_1:
  if (left < 1)
  {
     return 1; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || ((11 <= next[0]) && (next[0] <= 255))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  return -1;
  l1_2:
  if (left < 1)
  {
     return 2; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || ((next[0] == '.') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('@' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255)))))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_6;
  }
  return -1;
  l1_3:
  if (left < 1)
  {
     return 3; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_9;
  }
  return -1;
  l1_4:
  if (left < 1)
  {
     return 4; //accept
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || ((next[0] == '.') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('@' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255)))))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_5;
  }
  return -1;
  l1_5:
  if (left < 1)
  {
     return 5; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('A' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255))))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_5;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_2;
  }
  if (((left >= 1) && ((next[0] == '@') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_3;
  }
  return -1;
  l1_6:
  if (left < 1)
  {
     return 6; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('A' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255))))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_6;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_2;
  }
  if (((left >= 1) && ((next[0] == '@') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_3;
  }
  return -1;
  l1_7:
  if (left < 1)
  {
     return 7; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_11;
  }
  return -1;
  l1_8:
  if (left < 1)
  {
     return 8; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_8;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_9;
  }
  return -1;
  l1_9:
  if (left < 1)
  {
     return 9; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((next[0] == '/') || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_8;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_7;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_9;
  }
  return -1;
  l1_10:
  if (left < 1)
  {
     return 10; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_10;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_11;
  }
  return -1;
  l1_11:
  if (left < 1)
  {
     return 11; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((next[0] == '/') || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_10;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_7;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     next += 1;
     left -= 1;
     goto l1_11;
  }
  return -1;
  //accept1:
  //  return;
  //fail1:
  //  fprintf(stderr, "Match error at input symbol %zu, (next char: %u)!\n", count, next[0]);
  //  exit(1);
}

int silent_match(int phase, int start_state, unsigned char * buf, long length)
{
  switch(phase) {
    case 1:
      return silent_match1(start_state, buf, length);
      break;
    default:
      fprintf(stderr, "Invalid phase: %d given\n", phase);
      exit(1);
  }
}
