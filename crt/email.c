
#define NUM_PHASES 1
#define BUFFER_UNIT_T uint8_t
#include "crt.h"

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
}

void init_buffer(buffer_t *buf)
{
  buf->data = malloc(INITIAL_BUFFER_SIZE);
  buf->size = INITIAL_BUFFER_SIZE;
  buf->bitpos = 0;
  buf->data[0] = 0;
}

void destroy_buffer(buffer_t *buf)
{
  if (buf->data != NULL)
    free(buf->data);
  buf->data = NULL;
}

INLINE
void outputconst(buffer_unit_t w, size_t bits)
{
  if (buf_writeconst(&outbuf, w, bits))
  {
    buf_flush(&outbuf);
  }
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
  appendarray(dst, src->data, src->bitpos);
}

INLINE
void outputarray(const buffer_unit_t *arr, size_t bits)
{
  int word_count = bits / BUFFER_UNIT_BITS;
  // Write completed words
  size_t word_index = 0;
  for (word_index = 0; word_index < word_count; word_index++)
  {
    outputconst(arr[word_index], BUFFER_UNIT_BITS);
  }

  int remaining = bits % BUFFER_UNIT_BITS;
  if (remaining != 0)
  {
    outputconst(arr[bits / BUFFER_UNIT_BITS], remaining);
  }
}

INLINE
void output(buffer_t *buf)
{
  outputarray(buf->data, buf->bitpos);
}

INLINE
void consume(int c)
{
  count     += c;
  in_cursor += c;
  next      += c;
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

void flush_outbuf()
{
  if (outbuf.bitpos % BUFFER_UNIT_BITS != 0)
  {
    outputconst(0, BUFFER_UNIT_BITS);
  }
  buf_flush(&outbuf);
}

void init_outbuf()
{
  outbuf.size = OUTBUFFER_SIZE + BUFFER_UNIT_SIZE;
  outbuf.data = malloc(outbuf.size);
  reset(&outbuf);
}


/* no tables */

struct state state_table[] = {
  {.num =  0, .accepting = 0},
  {.num =  1, .accepting = 0},
  {.num =  2, .accepting = 0},
  {.num =  3, .accepting = 0},
  {.num =  4, .accepting = 1},
  {.num =  5, .accepting = 0},
  {.num =  6, .accepting = 0},
  {.num =  7, .accepting = 0},
  {.num =  8, .accepting = 0},
  {.num =  9, .accepting = 0},
  {.num =  10, .accepting = 0},
  {.num =  11, .accepting = 0}
};
int state_count = 12;

buffer_t buf_0;
buffer_t buf_1;
buffer_t buf_2;
buffer_t buf_3;
buffer_t buf_4;
buffer_t buf_5;
buffer_t buf_6;
buffer_t buf_7;
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
  fprintf(stdout, "No object file generated!\nOptions:\nSST optimization level:  3\nWord size:               UInt8T\nIdentity tables removed: False\n\nSource file: email.kex\nSource md5:  65b11bd751a334edf99e8f11582189f9\nSST states:  12\n");
}

void init()
{
init_buffer(&buf_1);
init_buffer(&buf_2);
init_buffer(&buf_3);
init_buffer(&buf_4);
init_buffer(&buf_5);
init_buffer(&buf_6);
init_buffer(&buf_7);
}

int match1(int phase, int start_state, char * buf, long length)
{
  int state = 0;
  long left = length;
  unsigned char * next = (unsigned char *)buf;
  if (start_state < 0) goto l1_0;
  switch (phase) {
    case 1:
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
  }
  l1_0:
  state = 0;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || ((next[0] == '.') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('@' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255)))))))))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     reset(&buf_1);
     append(&buf_1,next[0],8);
     next += 1; left -= 1;
     goto l1_5;
  }
  return -1;
  l1_1:
  state = 1;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || ((11 <= next[0]) && (next[0] <= 255))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  return -1;
  l1_2:
  state = 2;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || ((next[0] == '.') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('@' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255)))))))))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     append(&buf_1,next[0],8);
     next += 1; left -= 1;
     goto l1_6;
  }
  return -1;
  l1_3:
  state = 3;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     reset(&buf_5);
     reset(&buf_2);
     append(&buf_1,next[0],8);
     next += 1; left -= 1;
     goto l1_9;
  }
  return -1;
  l1_4:
  state = 4;
  if (left < 1)
  {
     return state; //accept
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || ((next[0] == '.') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('@' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255)))))))))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     reset(&buf_1);
     append(&buf_1,next[0],8);
     next += 1; left -= 1;
     goto l1_5;
  }
  return -1;
  l1_5:
  state = 5;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('A' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255))))))))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     append(&buf_1,next[0],8);
     next += 1; left -= 1;
     goto l1_5;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     appendarray(&buf_1,const_1_2,8);
     next += 1; left -= 1;
     goto l1_2;
  }
  if (((left >= 1) && ((next[0] == '@') && 1)))
  {
     appendarray(&buf_1,const_1_3,8);
     next += 1; left -= 1;
     goto l1_3;
  }
  return -1;
  l1_6:
  state = 6;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ' ')) || ((next[0] == '"') || ((('(' <= next[0]) && (next[0] <= ')')) || ((next[0] == ',') || (((':' <= next[0]) && (next[0] <= '<')) || ((next[0] == '>') || ((('A' <= next[0]) && (next[0] <= '[')) || ((next[0] == ']') || ((127 <= next[0]) && (next[0] <= 255))))))))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((next[0] == '!') || ((('#' <= next[0]) && (next[0] <= 39)) || ((('*' <= next[0]) && (next[0] <= '+')) || ((next[0] == '-') || ((('/' <= next[0]) && (next[0] <= '9')) || ((next[0] == '=') || ((next[0] == '?') || ((next[0] == 92) || (('^' <= next[0]) && (next[0] <= '~')))))))))) && 1)))
  {
     append(&buf_1,next[0],8);
     next += 1; left -= 1;
     goto l1_6;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     appendarray(&buf_1,const_1_2,8);
     next += 1; left -= 1;
     goto l1_2;
  }
  if (((left >= 1) && ((next[0] == '@') && 1)))
  {
     appendarray(&buf_1,const_1_3,8);
     next += 1; left -= 1;
     goto l1_3;
  }
  return -1;
  l1_7:
  state = 7;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     reset(&buf_7);
     reset(&buf_6);
     reset(&buf_5);
     append(&buf_5,next[0],8);
     reset(&buf_4);
     reset(&buf_3);
     reset(&buf_2);
     append(&buf_2,next[0],8);
     next += 1; left -= 1;
     goto l1_11;
  }
  return -1;
  l1_8:
  state = 8;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     appendarray(&buf_1,const_1_1,8);
     next += 1; left -= 1;
     goto l1_8;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     reset(&buf_5);
     append(&buf_5,next[0],8);
     reset(&buf_2);
     append(&buf_2,next[0],8);
     next += 1; left -= 1;
     goto l1_9;
  }
  return -1;
  l1_9:
  state = 9;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((next[0] == '/') || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     concat(&buf_1,&buf_2);
     appendarray(&buf_1,const_1_1,8);
     next += 1; left -= 1;
     goto l1_8;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     concat(&buf_1,&buf_5);
     appendarray(&buf_1,const_1_2,8);
     next += 1; left -= 1;
     goto l1_7;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     reset(&buf_5);
     append(&buf_5,next[0],8);
     concat(&buf_1,&buf_2);
     reset(&buf_2);
     append(&buf_2,next[0],8);
     next += 1; left -= 1;
     goto l1_9;
  }
  return -1;
  l1_10:
  state = 10;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     appendarray(&buf_5,const_1_1,8);
     appendarray(&buf_2,const_1_1,8);
     next += 1; left -= 1;
     goto l1_10;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     reset(&buf_7);
     append(&buf_7,next[0],8);
     reset(&buf_6);
     append(&buf_6,next[0],8);
     reset(&buf_4);
     append(&buf_4,next[0],8);
     reset(&buf_3);
     append(&buf_3,next[0],8);
     next += 1; left -= 1;
     goto l1_11;
  }
  return -1;
  l1_11:
  state = 11;
  if (left < 1)
  {
     return state; //fail
  }
  if (((left >= 1) && ((((0 <= next[0]) && (next[0] <= 9)) || (((11 <= next[0]) && (next[0] <= ',')) || ((next[0] == '/') || (((':' <= next[0]) && (next[0] <= '`')) || (('{' <= next[0]) && (next[0] <= 255)))))) && 1)))
  {
     next += 1; left -= 1;
     goto l1_1;
  }
  if (((left >= 1) && ((next[0] == 10) && 1)))
  {
     output(&buf_1);
     output(&buf_5);
     output(&buf_7);
     outputarray(const_1_0,8);
     next += 1; left -= 1;
     goto l1_4;
  }
  if (((left >= 1) && ((next[0] == '-') && 1)))
  {
     concat(&buf_5,&buf_4);
     appendarray(&buf_5,const_1_1,8);
     concat(&buf_2,&buf_3);
     appendarray(&buf_2,const_1_1,8);
     next += 1; left -= 1;
     goto l1_10;
  }
  if (((left >= 1) && ((next[0] == '.') && 1)))
  {
     concat(&buf_1,&buf_2);
     concat(&buf_1,&buf_6);
     appendarray(&buf_1,const_1_2,8);
     next += 1; left -= 1;
     goto l1_7;
  }
  if (((left >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || (('a' <= next[0]) && (next[0] <= 'z'))) && 1)))
  {
     reset(&buf_7);
     append(&buf_7,next[0],8);
     reset(&buf_6);
     append(&buf_6,next[0],8);
     concat(&buf_5,&buf_4);
     reset(&buf_4);
     append(&buf_4,next[0],8);
     concat(&buf_2,&buf_3);
     reset(&buf_3);
     append(&buf_3,next[0],8);
     next += 1; left -= 1;
     goto l1_11;
  }
  return -1;
  //accept1:
  //  return;
  //fail1:
  //  fprintf(stderr, "Match error at input symbol %zu, (next char: %u)!\n", count, next[0]);
  //  exit(1);
}

int match(int phase, int start_state, char * buf, long length)
{
  switch(phase) {
    case 1:
      return match1(phase, start_state, buf, length);
      break;
    default:
      fprintf(stderr, "Invalid phase: %d given\n", phase);
      exit(1);
  }
}
