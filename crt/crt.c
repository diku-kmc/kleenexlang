#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>

#define OUTBUFFER_SIZE 4096
#define INITIAL_BUFFER_SIZE (4096*8)

typedef %%BUFFER_UNIT_T buffer_unit_t;
typedef struct {
  buffer_unit_t *data;
  size_t size; /* size in bytes */
  size_t bitpos;
} buffer_t;

#define BUFFER_UNIT_SIZE (sizeof(buffer_unit_t))
#define BUFFER_UNIT_BITS (BUFFER_UNIT_SIZE * 8)

int next;
buffer_t outbuf;
size_t count = 0;

void buf_flush(buffer_t *buf)
{
  size_t word_index = buf->bitpos / BUFFER_UNIT_BITS;
  // If we do not have a single complete word to flush, return.
  // Not just an optimization! The zeroing logic below assumes word_index > 0.
  if (word_index == 0) return;
  if (fwrite(buf->data, BUFFER_UNIT_SIZE, word_index, stdout) == -1)
  {
    fprintf(stderr, "Error writing to stdout.\n");
    exit(1);
  }
  // Zeroing is important, as we "or" bit fragments into buffer.
  memset(buf->data, 0, word_index * BUFFER_UNIT_SIZE);

  // Since partially written words are not flushed, they need to be moved to the
  // beginning of the buffer.
  // Note: We assume word_index > 0 to avoid losing data!
  buf->data[0] = buf->data[word_index];
  // ... and then zeroed
  buf->data[word_index] = 0;

  // Rewind cursor
  buf->bitpos = buf->bitpos - word_index * BUFFER_UNIT_BITS;
}


// Write first 'bits' of 'w' to 'buf', starting from the MOST significant bit.
// Precondition: Remaining bits of 'w' must be zero.
static inline
bool buf_writeconst(buffer_t *buf, buffer_unit_t w, int bits)
{
  size_t word_index = buf->bitpos / BUFFER_UNIT_BITS;
  size_t offset = buf->bitpos % BUFFER_UNIT_BITS;
  size_t bits_available = BUFFER_UNIT_BITS - offset;

  buf->data[word_index] |= w >> offset;
  // Test important; shifting by the word size is undefined behaviour.
  if (offset > 0)
    buf->data[word_index+1] |= w << bits_available;

  buf->bitpos += bits;

  // Is cursor in last word?
  return (buf->bitpos >= buf->size * 8 - BUFFER_UNIT_BITS);
}

void buf_resize(buffer_t *buf, size_t shift)
{
  size_t new_size = buf->size << shift;
  buffer_unit_t *data2 = malloc(new_size);
  memset(data2, 0, new_size);
  memcpy(data2, buf->data, buf->size);
  free(buf->data);
  buf->data = data2;
  buf->size = new_size;
}

static inline
void buf_writearray(buffer_t *dst, buffer_unit_t *arr, int bits)
{
  if (dst->bitpos % BUFFER_UNIT_BITS == 0)
  {
    int count = (bits / BUFFER_UNIT_BITS) + (bits % BUFFER_UNIT_BITS ? 1 : 0);
    memcpy(dst->data, arr, count * BUFFER_UNIT_SIZE);
    dst->bitpos += bits;
  } else
  {
    int word_index = 0;
    for (word_index = 0; word_index <= bits / BUFFER_UNIT_BITS; word_index++)
    {
      buf_writeconst(dst, arr[word_index], BUFFER_UNIT_BITS);
    }

    if (bits % BUFFER_UNIT_BITS != 0)
    {
      buf_writeconst(dst, arr[word_index], bits % BUFFER_UNIT_BITS);
    }
  }
}

static inline
void reset(buffer_t *buf)
{
  memset(buf->data, 0, buf->bitpos / 8);
  if (buf->bitpos % BUFFER_UNIT_BITS != 0)
  {
    buf->data[buf->bitpos / BUFFER_UNIT_BITS] = 0;
  }
  buf->bitpos = 0;
}

void init_buffer(buffer_t *buf)
{
  buf->data = malloc(INITIAL_BUFFER_SIZE);
  buf->size = INITIAL_BUFFER_SIZE;
  buf->bitpos = 0;
  memset(buf->data, 0, buf->size);
}

static inline
void writeconst(buffer_unit_t w, int bits)
{
  if (buf_writeconst(&outbuf, w, bits))
  {
    buf_flush(&outbuf);
  }
}

static inline
void appendarray(buffer_t *buf, buffer_unit_t *arr, int bits)
{
  size_t total_bits = dst->bitpos + bits;
  if (total_bits >= (dst->size - 1) * BUFFER_UNIT_BITS)
  {
    size_t shift = 1;
    while (total_bits >= ((dst->size << shift) - 1) * BUFFER_UNIT_BITS)
      shift++;
    buf_resize(dst, shift);
  }

  buf_writearray(buf, arr, bits);
}

static inline
void append(buffer_t *buf, buffer_unit_t w, int bits)
{
  if (buf_writeconst(buf, w, bits))
    buf_resize(buf, 1);
}

static inline
void concat(buffer_t *dst, buffer_t *src)
{
  appendarray(dst, src->data, src->bitpos);
}

static inline
void write(buffer_t *buf)
{
  if (outbuf.bitpos % BUFFER_UNIT_BITS == 0)
  {
    buf_flush(&outbuf);
    buf_flush(buf);
    // Important that we fall through to the "handle remaining bits" case.
  }

  // Write completed words
  size_t word_index = 0;
  for (word_index = 0; word_index < buf->bitpos / BUFFER_UNIT_BITS; word_index++)
  {
    writeconst(buf->data[word_index], BUFFER_UNIT_BITS);
  }

  // Handle remaining bits
  if (buf->bitpos % BUFFER_UNIT_BITS != 0)
  {
    size_t remaining = buf->bitpos - (word_index * BUFFER_UNIT_BITS);
    writeconst(buf->data[word_index], remaining);
  }
}

static inline
int readnext()
{
  next = getchar();
  count++;
  return (next != EOF);
}

%%TABLES

%%DECLS

void match()
{
%%PROG
  accept:
    return;

  fail:
    fprintf(stderr, "Match error at input symbol %zu!\n", count);
    exit(1);
}


int main(int argc, char *argv[])
{
  outbuf.size = OUTBUFFER_SIZE + BUFFER_UNIT_SIZE;
  outbuf.data = malloc(outbuf.size);
  reset(&outbuf);

%%INIT

  match();

  if (outbuf.bitpos % BUFFER_UNIT_BITS != 0)
    writeconst(0, BUFFER_UNIT_BITS);
  buf_flush(&outbuf);
}
