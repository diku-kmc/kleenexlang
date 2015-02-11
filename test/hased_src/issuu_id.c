#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>

#define OUTBUFFER_SIZE 4096
#define INITIAL_BUFFER_SIZE (4096*8)

typedef uint8_t buffer_unit_t;
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


// Write firt 'bits' of 'w' to 'buf', starting from the MOST significant bit.
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
void append(buffer_t *buf, buffer_unit_t w, int bits)
{
  if (buf_writeconst(buf, w, bits))
    buf_resize(buf, 1);
}

static inline
void concat(buffer_t *dst, buffer_t *src)
{
  size_t word_index;
  for (word_index = 0; word_index < src->bitpos / BUFFER_UNIT_BITS; word_index++)
  {
    append(dst, src->data[word_index], BUFFER_UNIT_BITS);
  }

  if (src->bitpos % BUFFER_UNIT_BITS != 0)
  {
    size_t remaining = src->bitpos % BUFFER_UNIT_BITS;
    append(dst, src->data[word_index], remaining);
  }
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

uint8_t tbl[1][256] =
{{ 0x0,  0x1,  0x2,  0x3,  0x4,  0x5,  0x6,  0x7,
   0x8,  0x9,  0xa,  0xb,  0xc,  0xd,  0xe,  0xf,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
  0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff}};

buffer_t b0;
buffer_t b1;
buffer_t b2;
buffer_t b3;
buffer_t b4;
buffer_t b5;
buffer_t b6;
buffer_t b7;
buffer_t b8;
buffer_t b9;
buffer_t b10;
buffer_t b11;
buffer_t b12;
buffer_t b13;
buffer_t b14;

void match()
{
goto l0;
l0: if (!readnext())
    {
       goto fail;
    }
    if (((123 <= next) && (next <= 123)))
    {
       reset(&b1);
       append(&b1,0x7b,8);
       reset(&b8);
       append(&b8,0x7b,8);
       goto l2;
    }
    goto fail;
l1: if (!readnext())
    {
       write(&b8);
       goto accept;
    }
    if (((10 <= next) && (next <= 10)))
    {
       append(&b1,0xa,8);
       append(&b8,0xa,8);
       goto l3;
    }
    goto fail;
l2: if (!readnext())
    {
       goto fail;
    }
    if (((34 <= next) && (next <= 34)))
    {
       goto l508;
    }
    goto fail;
l3: if (!readnext())
    {
       write(&b8);
       goto accept;
    }
    if (((123 <= next) && (next <= 123)))
    {
       write(&b1);
       reset(&b1);
       append(&b1,0x7b,8);
       reset(&b8);
       append(&b8,0x7b,8);
       goto l2;
    }
    goto fail;
l4: if (!readnext())
    {
       goto fail;
    }
    if (((44 <= next) && (next <= 44)))
    {
       concat(&b1,&b2);
       append(&b1,0x2c,8);
       concat(&b8,&b5);
       append(&b8,0x2c,8);
       goto l2;
    }
    if (((125 <= next) && (next <= 125)))
    {
       concat(&b1,&b9);
       append(&b1,0x7d,8);
       concat(&b8,&b12);
       append(&b8,0x7d,8);
       goto l1;
    }
    goto fail;
l5: if (!readnext())
    {
       goto fail;
    }
    if (((44 <= next) && (next <= 44)))
    {
       concat(&b1,&b2);
       append(&b1,0x2c,8);
       concat(&b8,&b5);
       append(&b8,0x2c,8);
       goto l2;
    }
    if (((48 <= next) && (next <= 57)))
    {
       append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
       append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
       append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
       append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
       goto l5;
    }
    if (((125 <= next) && (next <= 125)))
    {
       concat(&b1,&b9);
       append(&b1,0x7d,8);
       concat(&b8,&b12);
       append(&b8,0x7d,8);
       goto l1;
    }
    goto fail;
l6: if (!readnext())
    {
       goto fail;
    }
    if (((48 <= next) && (next <= 57)))
    {
       reset(&b2);
       append(&b2,0x22,8);
       append(&b2,0x74,8);
       append(&b2,0x73,8);
       append(&b2,0x22,8);
       append(&b2,0x3a,8);
       append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
       reset(&b5);
       append(&b5,0x22,8);
       append(&b5,0x74,8);
       append(&b5,0x73,8);
       append(&b5,0x22,8);
       append(&b5,0x3a,8);
       append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
       reset(&b9);
       append(&b9,0x22,8);
       append(&b9,0x74,8);
       append(&b9,0x73,8);
       append(&b9,0x22,8);
       append(&b9,0x3a,8);
       append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
       reset(&b12);
       append(&b12,0x22,8);
       append(&b12,0x74,8);
       append(&b12,0x73,8);
       append(&b12,0x22,8);
       append(&b12,0x3a,8);
       append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
       goto l5;
    }
    goto fail;
l7: if (!readnext())
    {
       goto fail;
    }
    if (((58 <= next) && (next <= 58)))
    {
       goto l6;
    }
    goto fail;
l8: if (!readnext())
    {
       goto fail;
    }
    if (((34 <= next) && (next <= 34)))
    {
       goto l7;
    }
    goto fail;
l9: if (!readnext())
    {
       goto fail;
    }
    if (((115 <= next) && (next <= 115)))
    {
       goto l8;
    }
    goto fail;
l10: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        append(&b2,0x22,8);
        append(&b5,0x22,8);
        append(&b9,0x22,8);
        append(&b12,0x22,8);
        goto l4;
     }
     goto fail;
l11: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l10;
     }
     goto fail;
l12: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l11;
     }
     goto fail;
l13: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l12;
     }
     goto fail;
l14: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l13;
     }
     goto fail;
l15: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l14;
     }
     goto fail;
l16: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l15;
     }
     goto fail;
l17: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l16;
     }
     goto fail;
l18: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l17;
     }
     goto fail;
l19: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l18;
     }
     goto fail;
l20: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l19;
     }
     goto fail;
l21: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l20;
     }
     goto fail;
l22: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l21;
     }
     goto fail;
l23: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l22;
     }
     goto fail;
l24: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l23;
     }
     goto fail;
l25: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l24;
     }
     goto fail;
l26: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        reset(&b2);
        append(&b2,0x22,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x73,8);
        append(&b2,0x69,8);
        append(&b2,0x74,8);
        append(&b2,0x6f,8);
        append(&b2,0x72,8);
        append(&b2,0x5f,8);
        append(&b2,0x75,8);
        append(&b2,0x75,8);
        append(&b2,0x69,8);
        append(&b2,0x64,8);
        append(&b2,0x22,8);
        append(&b2,0x3a,8);
        append(&b2,0x22,8);
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        reset(&b5);
        append(&b5,0x22,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x73,8);
        append(&b5,0x69,8);
        append(&b5,0x74,8);
        append(&b5,0x6f,8);
        append(&b5,0x72,8);
        append(&b5,0x5f,8);
        append(&b5,0x75,8);
        append(&b5,0x75,8);
        append(&b5,0x69,8);
        append(&b5,0x64,8);
        append(&b5,0x22,8);
        append(&b5,0x3a,8);
        append(&b5,0x22,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        reset(&b9);
        append(&b9,0x22,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x73,8);
        append(&b9,0x69,8);
        append(&b9,0x74,8);
        append(&b9,0x6f,8);
        append(&b9,0x72,8);
        append(&b9,0x5f,8);
        append(&b9,0x75,8);
        append(&b9,0x75,8);
        append(&b9,0x69,8);
        append(&b9,0x64,8);
        append(&b9,0x22,8);
        append(&b9,0x3a,8);
        append(&b9,0x22,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        reset(&b12);
        append(&b12,0x22,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x73,8);
        append(&b12,0x69,8);
        append(&b12,0x74,8);
        append(&b12,0x6f,8);
        append(&b12,0x72,8);
        append(&b12,0x5f,8);
        append(&b12,0x75,8);
        append(&b12,0x75,8);
        append(&b12,0x69,8);
        append(&b12,0x64,8);
        append(&b12,0x22,8);
        append(&b12,0x3a,8);
        append(&b12,0x22,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l25;
     }
     goto fail;
l27: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l26;
     }
     goto fail;
l28: if (!readnext())
     {
        goto fail;
     }
     if (((58 <= next) && (next <= 58)))
     {
        goto l27;
     }
     goto fail;
l29: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l28;
     }
     goto fail;
l30: if (!readnext())
     {
        goto fail;
     }
     if (((100 <= next) && (next <= 100)))
     {
        goto l29;
     }
     goto fail;
l31: if (!readnext())
     {
        goto fail;
     }
     if (((105 <= next) && (next <= 105)))
     {
        goto l30;
     }
     goto fail;
l32: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        append(&b2,0x22,8);
        append(&b5,0x22,8);
        append(&b9,0x22,8);
        append(&b12,0x22,8);
        goto l4;
     }
     goto fail;
l33: if (!readnext())
     {
        goto fail;
     }
     if (((108 <= next) && (next <= 108)))
     {
        reset(&b2);
        append(&b2,0x22,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x73,8);
        append(&b2,0x69,8);
        append(&b2,0x74,8);
        append(&b2,0x6f,8);
        append(&b2,0x72,8);
        append(&b2,0x5f,8);
        append(&b2,0x73,8);
        append(&b2,0x6f,8);
        append(&b2,0x75,8);
        append(&b2,0x72,8);
        append(&b2,0x63,8);
        append(&b2,0x65,8);
        append(&b2,0x22,8);
        append(&b2,0x3a,8);
        append(&b2,0x22,8);
        append(&b2,0x65,8);
        append(&b2,0x78,8);
        append(&b2,0x74,8);
        append(&b2,0x65,8);
        append(&b2,0x72,8);
        append(&b2,0x6e,8);
        append(&b2,0x61,8);
        append(&b2,0x6c,8);
        reset(&b5);
        append(&b5,0x22,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x73,8);
        append(&b5,0x69,8);
        append(&b5,0x74,8);
        append(&b5,0x6f,8);
        append(&b5,0x72,8);
        append(&b5,0x5f,8);
        append(&b5,0x73,8);
        append(&b5,0x6f,8);
        append(&b5,0x75,8);
        append(&b5,0x72,8);
        append(&b5,0x63,8);
        append(&b5,0x65,8);
        append(&b5,0x22,8);
        append(&b5,0x3a,8);
        append(&b5,0x22,8);
        append(&b5,0x65,8);
        append(&b5,0x78,8);
        append(&b5,0x74,8);
        append(&b5,0x65,8);
        append(&b5,0x72,8);
        append(&b5,0x6e,8);
        append(&b5,0x61,8);
        append(&b5,0x6c,8);
        reset(&b9);
        append(&b9,0x22,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x73,8);
        append(&b9,0x69,8);
        append(&b9,0x74,8);
        append(&b9,0x6f,8);
        append(&b9,0x72,8);
        append(&b9,0x5f,8);
        append(&b9,0x73,8);
        append(&b9,0x6f,8);
        append(&b9,0x75,8);
        append(&b9,0x72,8);
        append(&b9,0x63,8);
        append(&b9,0x65,8);
        append(&b9,0x22,8);
        append(&b9,0x3a,8);
        append(&b9,0x22,8);
        append(&b9,0x65,8);
        append(&b9,0x78,8);
        append(&b9,0x74,8);
        append(&b9,0x65,8);
        append(&b9,0x72,8);
        append(&b9,0x6e,8);
        append(&b9,0x61,8);
        append(&b9,0x6c,8);
        reset(&b12);
        append(&b12,0x22,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x73,8);
        append(&b12,0x69,8);
        append(&b12,0x74,8);
        append(&b12,0x6f,8);
        append(&b12,0x72,8);
        append(&b12,0x5f,8);
        append(&b12,0x73,8);
        append(&b12,0x6f,8);
        append(&b12,0x75,8);
        append(&b12,0x72,8);
        append(&b12,0x63,8);
        append(&b12,0x65,8);
        append(&b12,0x22,8);
        append(&b12,0x3a,8);
        append(&b12,0x22,8);
        append(&b12,0x65,8);
        append(&b12,0x78,8);
        append(&b12,0x74,8);
        append(&b12,0x65,8);
        append(&b12,0x72,8);
        append(&b12,0x6e,8);
        append(&b12,0x61,8);
        append(&b12,0x6c,8);
        goto l32;
     }
     goto fail;
l34: if (!readnext())
     {
        goto fail;
     }
     if (((97 <= next) && (next <= 97)))
     {
        goto l33;
     }
     goto fail;
l35: if (!readnext())
     {
        goto fail;
     }
     if (((110 <= next) && (next <= 110)))
     {
        goto l34;
     }
     goto fail;
l36: if (!readnext())
     {
        goto fail;
     }
     if (((114 <= next) && (next <= 114)))
     {
        goto l35;
     }
     goto fail;
l37: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l36;
     }
     goto fail;
l38: if (!readnext())
     {
        goto fail;
     }
     if (((116 <= next) && (next <= 116)))
     {
        goto l37;
     }
     goto fail;
l39: if (!readnext())
     {
        goto fail;
     }
     if (((120 <= next) && (next <= 120)))
     {
        goto l38;
     }
     goto fail;
l40: if (!readnext())
     {
        goto fail;
     }
     if (((108 <= next) && (next <= 108)))
     {
        reset(&b2);
        append(&b2,0x22,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x73,8);
        append(&b2,0x69,8);
        append(&b2,0x74,8);
        append(&b2,0x6f,8);
        append(&b2,0x72,8);
        append(&b2,0x5f,8);
        append(&b2,0x73,8);
        append(&b2,0x6f,8);
        append(&b2,0x75,8);
        append(&b2,0x72,8);
        append(&b2,0x63,8);
        append(&b2,0x65,8);
        append(&b2,0x22,8);
        append(&b2,0x3a,8);
        append(&b2,0x22,8);
        append(&b2,0x69,8);
        append(&b2,0x6e,8);
        append(&b2,0x74,8);
        append(&b2,0x65,8);
        append(&b2,0x72,8);
        append(&b2,0x6e,8);
        append(&b2,0x61,8);
        append(&b2,0x6c,8);
        reset(&b5);
        append(&b5,0x22,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x73,8);
        append(&b5,0x69,8);
        append(&b5,0x74,8);
        append(&b5,0x6f,8);
        append(&b5,0x72,8);
        append(&b5,0x5f,8);
        append(&b5,0x73,8);
        append(&b5,0x6f,8);
        append(&b5,0x75,8);
        append(&b5,0x72,8);
        append(&b5,0x63,8);
        append(&b5,0x65,8);
        append(&b5,0x22,8);
        append(&b5,0x3a,8);
        append(&b5,0x22,8);
        append(&b5,0x69,8);
        append(&b5,0x6e,8);
        append(&b5,0x74,8);
        append(&b5,0x65,8);
        append(&b5,0x72,8);
        append(&b5,0x6e,8);
        append(&b5,0x61,8);
        append(&b5,0x6c,8);
        reset(&b9);
        append(&b9,0x22,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x73,8);
        append(&b9,0x69,8);
        append(&b9,0x74,8);
        append(&b9,0x6f,8);
        append(&b9,0x72,8);
        append(&b9,0x5f,8);
        append(&b9,0x73,8);
        append(&b9,0x6f,8);
        append(&b9,0x75,8);
        append(&b9,0x72,8);
        append(&b9,0x63,8);
        append(&b9,0x65,8);
        append(&b9,0x22,8);
        append(&b9,0x3a,8);
        append(&b9,0x22,8);
        append(&b9,0x69,8);
        append(&b9,0x6e,8);
        append(&b9,0x74,8);
        append(&b9,0x65,8);
        append(&b9,0x72,8);
        append(&b9,0x6e,8);
        append(&b9,0x61,8);
        append(&b9,0x6c,8);
        reset(&b12);
        append(&b12,0x22,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x73,8);
        append(&b12,0x69,8);
        append(&b12,0x74,8);
        append(&b12,0x6f,8);
        append(&b12,0x72,8);
        append(&b12,0x5f,8);
        append(&b12,0x73,8);
        append(&b12,0x6f,8);
        append(&b12,0x75,8);
        append(&b12,0x72,8);
        append(&b12,0x63,8);
        append(&b12,0x65,8);
        append(&b12,0x22,8);
        append(&b12,0x3a,8);
        append(&b12,0x22,8);
        append(&b12,0x69,8);
        append(&b12,0x6e,8);
        append(&b12,0x74,8);
        append(&b12,0x65,8);
        append(&b12,0x72,8);
        append(&b12,0x6e,8);
        append(&b12,0x61,8);
        append(&b12,0x6c,8);
        goto l32;
     }
     goto fail;
l41: if (!readnext())
     {
        goto fail;
     }
     if (((97 <= next) && (next <= 97)))
     {
        goto l40;
     }
     goto fail;
l42: if (!readnext())
     {
        goto fail;
     }
     if (((110 <= next) && (next <= 110)))
     {
        goto l41;
     }
     goto fail;
l43: if (!readnext())
     {
        goto fail;
     }
     if (((114 <= next) && (next <= 114)))
     {
        goto l42;
     }
     goto fail;
l44: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l43;
     }
     goto fail;
l45: if (!readnext())
     {
        goto fail;
     }
     if (((116 <= next) && (next <= 116)))
     {
        goto l44;
     }
     goto fail;
l46: if (!readnext())
     {
        goto fail;
     }
     if (((110 <= next) && (next <= 110)))
     {
        goto l45;
     }
     goto fail;
l47: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l39;
     }
     if (((105 <= next) && (next <= 105)))
     {
        goto l46;
     }
     goto fail;
l48: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l47;
     }
     goto fail;
l49: if (!readnext())
     {
        goto fail;
     }
     if (((58 <= next) && (next <= 58)))
     {
        goto l48;
     }
     goto fail;
l50: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l49;
     }
     goto fail;
l51: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l50;
     }
     goto fail;
l52: if (!readnext())
     {
        goto fail;
     }
     if (((99 <= next) && (next <= 99)))
     {
        goto l51;
     }
     goto fail;
l53: if (!readnext())
     {
        goto fail;
     }
     if (((114 <= next) && (next <= 114)))
     {
        goto l52;
     }
     goto fail;
l54: if (!readnext())
     {
        goto fail;
     }
     if (((117 <= next) && (next <= 117)))
     {
        goto l53;
     }
     goto fail;
l55: if (!readnext())
     {
        goto fail;
     }
     if (((111 <= next) && (next <= 111)))
     {
        goto l54;
     }
     goto fail;
l56: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        append(&b2,0x22,8);
        append(&b5,0x22,8);
        append(&b9,0x22,8);
        append(&b12,0x22,8);
        goto l4;
     }
     goto fail;
l57: if (!readnext())
     {
        goto fail;
     }
     if (((114 <= next) && (next <= 114)))
     {
        reset(&b2);
        append(&b2,0x22,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x73,8);
        append(&b2,0x69,8);
        append(&b2,0x74,8);
        append(&b2,0x6f,8);
        append(&b2,0x72,8);
        append(&b2,0x5f,8);
        append(&b2,0x64,8);
        append(&b2,0x65,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x63,8);
        append(&b2,0x65,8);
        append(&b2,0x22,8);
        append(&b2,0x3a,8);
        append(&b2,0x22,8);
        append(&b2,0x62,8);
        append(&b2,0x72,8);
        append(&b2,0x6f,8);
        append(&b2,0x77,8);
        append(&b2,0x73,8);
        append(&b2,0x65,8);
        append(&b2,0x72,8);
        reset(&b5);
        append(&b5,0x22,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x73,8);
        append(&b5,0x69,8);
        append(&b5,0x74,8);
        append(&b5,0x6f,8);
        append(&b5,0x72,8);
        append(&b5,0x5f,8);
        append(&b5,0x64,8);
        append(&b5,0x65,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x63,8);
        append(&b5,0x65,8);
        append(&b5,0x22,8);
        append(&b5,0x3a,8);
        append(&b5,0x22,8);
        append(&b5,0x62,8);
        append(&b5,0x72,8);
        append(&b5,0x6f,8);
        append(&b5,0x77,8);
        append(&b5,0x73,8);
        append(&b5,0x65,8);
        append(&b5,0x72,8);
        reset(&b9);
        append(&b9,0x22,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x73,8);
        append(&b9,0x69,8);
        append(&b9,0x74,8);
        append(&b9,0x6f,8);
        append(&b9,0x72,8);
        append(&b9,0x5f,8);
        append(&b9,0x64,8);
        append(&b9,0x65,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x63,8);
        append(&b9,0x65,8);
        append(&b9,0x22,8);
        append(&b9,0x3a,8);
        append(&b9,0x22,8);
        append(&b9,0x62,8);
        append(&b9,0x72,8);
        append(&b9,0x6f,8);
        append(&b9,0x77,8);
        append(&b9,0x73,8);
        append(&b9,0x65,8);
        append(&b9,0x72,8);
        reset(&b12);
        append(&b12,0x22,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x73,8);
        append(&b12,0x69,8);
        append(&b12,0x74,8);
        append(&b12,0x6f,8);
        append(&b12,0x72,8);
        append(&b12,0x5f,8);
        append(&b12,0x64,8);
        append(&b12,0x65,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x63,8);
        append(&b12,0x65,8);
        append(&b12,0x22,8);
        append(&b12,0x3a,8);
        append(&b12,0x22,8);
        append(&b12,0x62,8);
        append(&b12,0x72,8);
        append(&b12,0x6f,8);
        append(&b12,0x77,8);
        append(&b12,0x73,8);
        append(&b12,0x65,8);
        append(&b12,0x72,8);
        goto l56;
     }
     goto fail;
l58: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l57;
     }
     goto fail;
l59: if (!readnext())
     {
        goto fail;
     }
     if (((115 <= next) && (next <= 115)))
     {
        goto l58;
     }
     goto fail;
l60: if (!readnext())
     {
        goto fail;
     }
     if (((119 <= next) && (next <= 119)))
     {
        goto l59;
     }
     goto fail;
l61: if (!readnext())
     {
        goto fail;
     }
     if (((111 <= next) && (next <= 111)))
     {
        goto l60;
     }
     goto fail;
l62: if (!readnext())
     {
        goto fail;
     }
     if (((114 <= next) && (next <= 114)))
     {
        goto l61;
     }
     goto fail;
l63: if (!readnext())
     {
        goto fail;
     }
     if (((100 <= next) && (next <= 100)))
     {
        reset(&b2);
        append(&b2,0x22,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x73,8);
        append(&b2,0x69,8);
        append(&b2,0x74,8);
        append(&b2,0x6f,8);
        append(&b2,0x72,8);
        append(&b2,0x5f,8);
        append(&b2,0x64,8);
        append(&b2,0x65,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x63,8);
        append(&b2,0x65,8);
        append(&b2,0x22,8);
        append(&b2,0x3a,8);
        append(&b2,0x22,8);
        append(&b2,0x61,8);
        append(&b2,0x6e,8);
        append(&b2,0x64,8);
        append(&b2,0x72,8);
        append(&b2,0x6f,8);
        append(&b2,0x69,8);
        append(&b2,0x64,8);
        reset(&b5);
        append(&b5,0x22,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x73,8);
        append(&b5,0x69,8);
        append(&b5,0x74,8);
        append(&b5,0x6f,8);
        append(&b5,0x72,8);
        append(&b5,0x5f,8);
        append(&b5,0x64,8);
        append(&b5,0x65,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x63,8);
        append(&b5,0x65,8);
        append(&b5,0x22,8);
        append(&b5,0x3a,8);
        append(&b5,0x22,8);
        append(&b5,0x61,8);
        append(&b5,0x6e,8);
        append(&b5,0x64,8);
        append(&b5,0x72,8);
        append(&b5,0x6f,8);
        append(&b5,0x69,8);
        append(&b5,0x64,8);
        reset(&b9);
        append(&b9,0x22,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x73,8);
        append(&b9,0x69,8);
        append(&b9,0x74,8);
        append(&b9,0x6f,8);
        append(&b9,0x72,8);
        append(&b9,0x5f,8);
        append(&b9,0x64,8);
        append(&b9,0x65,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x63,8);
        append(&b9,0x65,8);
        append(&b9,0x22,8);
        append(&b9,0x3a,8);
        append(&b9,0x22,8);
        append(&b9,0x61,8);
        append(&b9,0x6e,8);
        append(&b9,0x64,8);
        append(&b9,0x72,8);
        append(&b9,0x6f,8);
        append(&b9,0x69,8);
        append(&b9,0x64,8);
        reset(&b12);
        append(&b12,0x22,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x73,8);
        append(&b12,0x69,8);
        append(&b12,0x74,8);
        append(&b12,0x6f,8);
        append(&b12,0x72,8);
        append(&b12,0x5f,8);
        append(&b12,0x64,8);
        append(&b12,0x65,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x63,8);
        append(&b12,0x65,8);
        append(&b12,0x22,8);
        append(&b12,0x3a,8);
        append(&b12,0x22,8);
        append(&b12,0x61,8);
        append(&b12,0x6e,8);
        append(&b12,0x64,8);
        append(&b12,0x72,8);
        append(&b12,0x6f,8);
        append(&b12,0x69,8);
        append(&b12,0x64,8);
        goto l56;
     }
     goto fail;
l64: if (!readnext())
     {
        goto fail;
     }
     if (((105 <= next) && (next <= 105)))
     {
        goto l63;
     }
     goto fail;
l65: if (!readnext())
     {
        goto fail;
     }
     if (((111 <= next) && (next <= 111)))
     {
        goto l64;
     }
     goto fail;
l66: if (!readnext())
     {
        goto fail;
     }
     if (((114 <= next) && (next <= 114)))
     {
        goto l65;
     }
     goto fail;
l67: if (!readnext())
     {
        goto fail;
     }
     if (((100 <= next) && (next <= 100)))
     {
        goto l66;
     }
     goto fail;
l68: if (!readnext())
     {
        goto fail;
     }
     if (((110 <= next) && (next <= 110)))
     {
        goto l67;
     }
     goto fail;
l69: if (!readnext())
     {
        goto fail;
     }
     if (((97 <= next) && (next <= 97)))
     {
        goto l68;
     }
     if (((98 <= next) && (next <= 98)))
     {
        goto l62;
     }
     goto fail;
l70: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l69;
     }
     goto fail;
l71: if (!readnext())
     {
        goto fail;
     }
     if (((58 <= next) && (next <= 58)))
     {
        goto l70;
     }
     goto fail;
l72: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l71;
     }
     goto fail;
l73: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l72;
     }
     goto fail;
l74: if (!readnext())
     {
        goto fail;
     }
     if (((99 <= next) && (next <= 99)))
     {
        goto l73;
     }
     goto fail;
l75: if (!readnext())
     {
        goto fail;
     }
     if (((105 <= next) && (next <= 105)))
     {
        goto l74;
     }
     goto fail;
l76: if (!readnext())
     {
        goto fail;
     }
     if (((118 <= next) && (next <= 118)))
     {
        goto l75;
     }
     goto fail;
l77: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l76;
     }
     goto fail;
l78: if (!readnext())
     {
        goto fail;
     }
     if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l78;
     }
     if (((34 <= next) && (next <= 34)))
     {
        append(&b2,0x22,8);
        append(&b5,0x22,8);
        append(&b9,0x22,8);
        append(&b12,0x22,8);
        goto l4;
     }
     goto fail;
l79: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        reset(&b2);
        append(&b2,0x22,8);
        append(&b2,0x76,8);
        append(&b2,0x69,8);
        append(&b2,0x73,8);
        append(&b2,0x69,8);
        append(&b2,0x74,8);
        append(&b2,0x6f,8);
        append(&b2,0x72,8);
        append(&b2,0x5f,8);
        append(&b2,0x75,8);
        append(&b2,0x73,8);
        append(&b2,0x65,8);
        append(&b2,0x72,8);
        append(&b2,0x61,8);
        append(&b2,0x67,8);
        append(&b2,0x65,8);
        append(&b2,0x6e,8);
        append(&b2,0x74,8);
        append(&b2,0x22,8);
        append(&b2,0x3a,8);
        append(&b2,0x22,8);
        reset(&b5);
        append(&b5,0x22,8);
        append(&b5,0x76,8);
        append(&b5,0x69,8);
        append(&b5,0x73,8);
        append(&b5,0x69,8);
        append(&b5,0x74,8);
        append(&b5,0x6f,8);
        append(&b5,0x72,8);
        append(&b5,0x5f,8);
        append(&b5,0x75,8);
        append(&b5,0x73,8);
        append(&b5,0x65,8);
        append(&b5,0x72,8);
        append(&b5,0x61,8);
        append(&b5,0x67,8);
        append(&b5,0x65,8);
        append(&b5,0x6e,8);
        append(&b5,0x74,8);
        append(&b5,0x22,8);
        append(&b5,0x3a,8);
        append(&b5,0x22,8);
        reset(&b9);
        append(&b9,0x22,8);
        append(&b9,0x76,8);
        append(&b9,0x69,8);
        append(&b9,0x73,8);
        append(&b9,0x69,8);
        append(&b9,0x74,8);
        append(&b9,0x6f,8);
        append(&b9,0x72,8);
        append(&b9,0x5f,8);
        append(&b9,0x75,8);
        append(&b9,0x73,8);
        append(&b9,0x65,8);
        append(&b9,0x72,8);
        append(&b9,0x61,8);
        append(&b9,0x67,8);
        append(&b9,0x65,8);
        append(&b9,0x6e,8);
        append(&b9,0x74,8);
        append(&b9,0x22,8);
        append(&b9,0x3a,8);
        append(&b9,0x22,8);
        reset(&b12);
        append(&b12,0x22,8);
        append(&b12,0x76,8);
        append(&b12,0x69,8);
        append(&b12,0x73,8);
        append(&b12,0x69,8);
        append(&b12,0x74,8);
        append(&b12,0x6f,8);
        append(&b12,0x72,8);
        append(&b12,0x5f,8);
        append(&b12,0x75,8);
        append(&b12,0x73,8);
        append(&b12,0x65,8);
        append(&b12,0x72,8);
        append(&b12,0x61,8);
        append(&b12,0x67,8);
        append(&b12,0x65,8);
        append(&b12,0x6e,8);
        append(&b12,0x74,8);
        append(&b12,0x22,8);
        append(&b12,0x3a,8);
        append(&b12,0x22,8);
        goto l78;
     }
     goto fail;
l80: if (!readnext())
     {
        goto fail;
     }
     if (((58 <= next) && (next <= 58)))
     {
        goto l79;
     }
     goto fail;
l81: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        goto l80;
     }
     goto fail;
l82: if (!readnext())
     {
        goto fail;
     }
     if (((116 <= next) && (next <= 116)))
     {
        goto l81;
     }
     goto fail;
l83: if (!readnext())
     {
        goto fail;
     }
     if (((110 <= next) && (next <= 110)))
     {
        goto l82;
     }
     goto fail;
l84: if (!readnext())
     {
        goto fail;
     }
     if (((101 <= next) && (next <= 101)))
     {
        goto l83;
     }
     goto fail;
l85: if (!readnext())
     {
        goto fail;
     }
     if (((103 <= next) && (next <= 103)))
     {
        goto l84;
     }
     goto fail;
l86: if (!readnext())
     {
        goto fail;
     }
     if (((34 <= next) && (next <= 34)))
     {
        append(&b2,0x22,8);
        append(&b5,0x22,8);
        append(&b9,0x22,8);
        append(&b12,0x22,8);
        goto l4;
     }
     goto fail;
l87: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l86;
     }
     goto fail;
l88: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l87;
     }
     goto fail;
l89: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l88;
     }
     goto fail;
l90: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l89;
     }
     goto fail;
l91: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l90;
     }
     goto fail;
l92: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l91;
     }
     goto fail;
l93: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l92;
     }
     goto fail;
l94: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l93;
     }
     goto fail;
l95: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l94;
     }
     goto fail;
l96: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l95;
     }
     goto fail;
l97: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l96;
     }
     goto fail;
l98: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l97;
     }
     goto fail;
l99: if (!readnext())
     {
        goto fail;
     }
     if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
     {
        append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
        append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
        goto l98;
     }
     goto fail;
l100: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l99;
      }
      goto fail;
l101: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l100;
      }
      goto fail;
l102: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x6f,8);
         append(&b2,0x72,8);
         append(&b2,0x5f,8);
         append(&b2,0x69,8);
         append(&b2,0x70,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x6f,8);
         append(&b5,0x72,8);
         append(&b5,0x5f,8);
         append(&b5,0x69,8);
         append(&b5,0x70,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x6f,8);
         append(&b9,0x72,8);
         append(&b9,0x5f,8);
         append(&b9,0x69,8);
         append(&b9,0x70,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x6f,8);
         append(&b12,0x72,8);
         append(&b12,0x5f,8);
         append(&b12,0x69,8);
         append(&b12,0x70,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l101;
      }
      goto fail;
l103: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l102;
      }
      goto fail;
l104: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l103;
      }
      goto fail;
l105: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l104;
      }
      goto fail;
l106: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l105;
      }
      goto fail;
l107: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l108: if (!readnext())
      {
         goto fail;
      }
      if (((65 <= next) && (next <= 90)))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l107;
      }
      goto fail;
l109: if (!readnext())
      {
         goto fail;
      }
      if (((65 <= next) && (next <= 90)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x6f,8);
         append(&b2,0x72,8);
         append(&b2,0x5f,8);
         append(&b2,0x63,8);
         append(&b2,0x6f,8);
         append(&b2,0x75,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x72,8);
         append(&b2,0x79,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x6f,8);
         append(&b5,0x72,8);
         append(&b5,0x5f,8);
         append(&b5,0x63,8);
         append(&b5,0x6f,8);
         append(&b5,0x75,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x72,8);
         append(&b5,0x79,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x6f,8);
         append(&b9,0x72,8);
         append(&b9,0x5f,8);
         append(&b9,0x63,8);
         append(&b9,0x6f,8);
         append(&b9,0x75,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x72,8);
         append(&b9,0x79,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x6f,8);
         append(&b12,0x72,8);
         append(&b12,0x5f,8);
         append(&b12,0x63,8);
         append(&b12,0x6f,8);
         append(&b12,0x75,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x72,8);
         append(&b12,0x79,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l108;
      }
      goto fail;
l110: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l109;
      }
      goto fail;
l111: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l110;
      }
      goto fail;
l112: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l111;
      }
      goto fail;
l113: if (!readnext())
      {
         goto fail;
      }
      if (((121 <= next) && (next <= 121)))
      {
         goto l112;
      }
      goto fail;
l114: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l113;
      }
      goto fail;
l115: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l114;
      }
      goto fail;
l116: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l115;
      }
      goto fail;
l117: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l116;
      }
      goto fail;
l118: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l117;
      }
      goto fail;
l119: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l120: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l119;
      }
      goto fail;
l121: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l120;
      }
      goto fail;
l122: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l121;
      }
      goto fail;
l123: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l122;
      }
      goto fail;
l124: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l123;
      }
      goto fail;
l125: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l124;
      }
      goto fail;
l126: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l125;
      }
      goto fail;
l127: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l126;
      }
      goto fail;
l128: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l127;
      }
      goto fail;
l129: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l128;
      }
      goto fail;
l130: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l129;
      }
      goto fail;
l131: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l130;
      }
      goto fail;
l132: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l131;
      }
      goto fail;
l133: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l132;
      }
      goto fail;
l134: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l133;
      }
      goto fail;
l135: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x6f,8);
         append(&b2,0x72,8);
         append(&b2,0x5f,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x66,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x6f,8);
         append(&b5,0x72,8);
         append(&b5,0x5f,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x66,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x6f,8);
         append(&b9,0x72,8);
         append(&b9,0x5f,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x66,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x6f,8);
         append(&b12,0x72,8);
         append(&b12,0x5f,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x66,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l134;
      }
      goto fail;
l136: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l135;
      }
      goto fail;
l137: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l136;
      }
      goto fail;
l138: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l137;
      }
      goto fail;
l139: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l138;
      }
      goto fail;
l140: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l139;
      }
      goto fail;
l141: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l140;
      }
      goto fail;
l142: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l141;
      }
      goto fail;
l143: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l142;
      }
      goto fail;
l144: if (!readnext())
      {
         goto fail;
      }
      if (((102 <= next) && (next <= 102)))
      {
         goto l143;
      }
      goto fail;
l145: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l144;
      }
      goto fail;
l146: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l147: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         goto l146;
      }
      goto fail;
l148: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l147;
      }
      goto fail;
l149: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l148;
      }
      goto fail;
l150: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l149;
      }
      goto fail;
l151: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l150;
      }
      goto fail;
l152: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x74,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x61,8);
         append(&b2,0x6d,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x74,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x61,8);
         append(&b5,0x6d,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x74,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x61,8);
         append(&b9,0x6d,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x74,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x61,8);
         append(&b12,0x6d,8);
         goto l146;
      }
      goto fail;
l153: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l152;
      }
      goto fail;
l154: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l153;
      }
      goto fail;
l155: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l154;
      }
      goto fail;
l156: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l155;
      }
      goto fail;
l157: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x77,8);
         append(&b2,0x65,8);
         append(&b2,0x62,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x65,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x77,8);
         append(&b5,0x65,8);
         append(&b5,0x62,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x65,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x77,8);
         append(&b9,0x65,8);
         append(&b9,0x62,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x65,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x77,8);
         append(&b12,0x65,8);
         append(&b12,0x62,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x65,8);
         goto l146;
      }
      goto fail;
l158: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l157;
      }
      goto fail;
l159: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l158;
      }
      goto fail;
l160: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l159;
      }
      goto fail;
l161: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l160;
      }
      goto fail;
l162: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l161;
      }
      goto fail;
l163: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l151;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l156;
      }
      if (((119 <= next) && (next <= 119)))
      {
         goto l162;
      }
      goto fail;
l164: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l163;
      }
      goto fail;
l165: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l164;
      }
      goto fail;
l166: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l165;
      }
      goto fail;
l167: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l166;
      }
      goto fail;
l168: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l167;
      }
      goto fail;
l169: if (!readnext())
      {
         goto fail;
      }
      if (((121 <= next) && (next <= 121)))
      {
         goto l168;
      }
      goto fail;
l170: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l170;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l171: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x64,8);
         append(&b2,0x6f,8);
         append(&b2,0x63,8);
         append(&b2,0x5f,8);
         append(&b2,0x69,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x64,8);
         append(&b5,0x6f,8);
         append(&b5,0x63,8);
         append(&b5,0x5f,8);
         append(&b5,0x69,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x64,8);
         append(&b9,0x6f,8);
         append(&b9,0x63,8);
         append(&b9,0x5f,8);
         append(&b9,0x69,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x64,8);
         append(&b12,0x6f,8);
         append(&b12,0x63,8);
         append(&b12,0x5f,8);
         append(&b12,0x69,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l170;
      }
      goto fail;
l172: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l171;
      }
      goto fail;
l173: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l172;
      }
      goto fail;
l174: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l173;
      }
      goto fail;
l175: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l174;
      }
      goto fail;
l176: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l175;
      }
      goto fail;
l177: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l176;
      }
      goto fail;
l178: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l177;
      }
      goto fail;
l179: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l180: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x69,8);
         append(&b2,0x6d,8);
         append(&b2,0x70,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x73,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x69,8);
         append(&b5,0x6d,8);
         append(&b5,0x70,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x73,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x69,8);
         append(&b9,0x6d,8);
         append(&b9,0x70,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x73,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x69,8);
         append(&b12,0x6d,8);
         append(&b12,0x70,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x73,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         goto l179;
      }
      goto fail;
l181: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l180;
      }
      goto fail;
l182: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l181;
      }
      goto fail;
l183: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l182;
      }
      goto fail;
l184: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l183;
      }
      goto fail;
l185: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l184;
      }
      goto fail;
l186: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l185;
      }
      goto fail;
l187: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l186;
      }
      goto fail;
l188: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l187;
      }
      goto fail;
l189: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x6c,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x6b,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x6c,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x6b,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x6c,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x6b,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x6c,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x6b,8);
         goto l179;
      }
      goto fail;
l190: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l189;
      }
      goto fail;
l191: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l190;
      }
      goto fail;
l192: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         goto l179;
      }
      goto fail;
l193: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l192;
      }
      goto fail;
l194: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l193;
      }
      goto fail;
l195: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x64,8);
         append(&b2,0x6f,8);
         append(&b2,0x77,8);
         append(&b2,0x6e,8);
         append(&b2,0x6c,8);
         append(&b2,0x6f,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x64,8);
         append(&b5,0x6f,8);
         append(&b5,0x77,8);
         append(&b5,0x6e,8);
         append(&b5,0x6c,8);
         append(&b5,0x6f,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x64,8);
         append(&b9,0x6f,8);
         append(&b9,0x77,8);
         append(&b9,0x6e,8);
         append(&b9,0x6c,8);
         append(&b9,0x6f,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x64,8);
         append(&b12,0x6f,8);
         append(&b12,0x77,8);
         append(&b12,0x6e,8);
         append(&b12,0x6c,8);
         append(&b12,0x6f,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         goto l179;
      }
      goto fail;
l196: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l195;
      }
      goto fail;
l197: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l196;
      }
      goto fail;
l198: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l197;
      }
      goto fail;
l199: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l198;
      }
      goto fail;
l200: if (!readnext())
      {
         goto fail;
      }
      if (((119 <= next) && (next <= 119)))
      {
         goto l199;
      }
      goto fail;
l201: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l200;
      }
      goto fail;
l202: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x68,8);
         append(&b2,0x61,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x68,8);
         append(&b5,0x61,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x68,8);
         append(&b9,0x61,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x68,8);
         append(&b12,0x61,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         goto l179;
      }
      goto fail;
l203: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l202;
      }
      goto fail;
l204: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l203;
      }
      goto fail;
l205: if (!readnext())
      {
         goto fail;
      }
      if (((104 <= next) && (next <= 104)))
      {
         goto l204;
      }
      goto fail;
l206: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x70,8);
         append(&b2,0x61,8);
         append(&b2,0x67,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         append(&b2,0x74,8);
         append(&b2,0x69,8);
         append(&b2,0x6d,8);
         append(&b2,0x65,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x70,8);
         append(&b5,0x61,8);
         append(&b5,0x67,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         append(&b5,0x74,8);
         append(&b5,0x69,8);
         append(&b5,0x6d,8);
         append(&b5,0x65,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x70,8);
         append(&b9,0x61,8);
         append(&b9,0x67,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         append(&b9,0x74,8);
         append(&b9,0x69,8);
         append(&b9,0x6d,8);
         append(&b9,0x65,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x70,8);
         append(&b12,0x61,8);
         append(&b12,0x67,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         append(&b12,0x74,8);
         append(&b12,0x69,8);
         append(&b12,0x6d,8);
         append(&b12,0x65,8);
         goto l179;
      }
      goto fail;
l207: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l206;
      }
      goto fail;
l208: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l207;
      }
      goto fail;
l209: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x75,8);
         append(&b2,0x61,8);
         append(&b2,0x74,8);
         append(&b2,0x69,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         append(&b2,0x5f,8);
         append(&b2,0x6c,8);
         append(&b2,0x6f,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x75,8);
         append(&b5,0x61,8);
         append(&b5,0x74,8);
         append(&b5,0x69,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         append(&b5,0x5f,8);
         append(&b5,0x6c,8);
         append(&b5,0x6f,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x75,8);
         append(&b9,0x61,8);
         append(&b9,0x74,8);
         append(&b9,0x69,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         append(&b9,0x5f,8);
         append(&b9,0x6c,8);
         append(&b9,0x6f,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x75,8);
         append(&b12,0x61,8);
         append(&b12,0x74,8);
         append(&b12,0x69,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         append(&b12,0x5f,8);
         append(&b12,0x6c,8);
         append(&b12,0x6f,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         goto l179;
      }
      goto fail;
l210: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l209;
      }
      goto fail;
l211: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l210;
      }
      goto fail;
l212: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l211;
      }
      goto fail;
l213: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l212;
      }
      goto fail;
l214: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l213;
      }
      goto fail;
l215: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l214;
      }
      goto fail;
l216: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l215;
      }
      goto fail;
l217: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l216;
      }
      goto fail;
l218: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l217;
      }
      goto fail;
l219: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l218;
      }
      goto fail;
l220: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l219;
      }
      goto fail;
l221: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l220;
      }
      goto fail;
l222: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l221;
      }
      goto fail;
l223: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l222;
      }
      goto fail;
l224: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l526;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l201;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l188;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l533;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l194;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l205;
      }
      goto fail;
l225: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l224;
      }
      goto fail;
l226: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l225;
      }
      goto fail;
l227: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l226;
      }
      goto fail;
l228: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l227;
      }
      goto fail;
l229: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l228;
      }
      goto fail;
l230: if (!readnext())
      {
         goto fail;
      }
      if (((121 <= next) && (next <= 121)))
      {
         goto l229;
      }
      goto fail;
l231: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l232: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x64,8);
         append(&b2,0x6f,8);
         append(&b2,0x63,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x64,8);
         append(&b5,0x6f,8);
         append(&b5,0x63,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x64,8);
         append(&b9,0x6f,8);
         append(&b9,0x63,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x64,8);
         append(&b12,0x6f,8);
         append(&b12,0x63,8);
         goto l231;
      }
      goto fail;
l233: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l232;
      }
      goto fail;
l234: if (!readnext())
      {
         goto fail;
      }
      if (((120 <= next) && (next <= 120)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x66,8);
         append(&b2,0x6f,8);
         append(&b2,0x62,8);
         append(&b2,0x6f,8);
         append(&b2,0x78,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x66,8);
         append(&b5,0x6f,8);
         append(&b5,0x62,8);
         append(&b5,0x6f,8);
         append(&b5,0x78,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x66,8);
         append(&b9,0x6f,8);
         append(&b9,0x62,8);
         append(&b9,0x6f,8);
         append(&b9,0x78,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x66,8);
         append(&b12,0x6f,8);
         append(&b12,0x62,8);
         append(&b12,0x6f,8);
         append(&b12,0x78,8);
         goto l231;
      }
      goto fail;
l235: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l234;
      }
      goto fail;
l236: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l235;
      }
      goto fail;
l237: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l236;
      }
      goto fail;
l238: if (!readnext())
      {
         goto fail;
      }
      if (((102 <= next) && (next <= 102)))
      {
         goto l237;
      }
      goto fail;
l239: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l238;
      }
      goto fail;
l240: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x6c,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x6b,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x6c,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x6b,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x6c,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x6b,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x6c,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x6b,8);
         goto l231;
      }
      goto fail;
l241: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l240;
      }
      goto fail;
l242: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l241;
      }
      goto fail;
l243: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l233;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l239;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l242;
      }
      goto fail;
l244: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l243;
      }
      goto fail;
l245: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l244;
      }
      goto fail;
l246: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l245;
      }
      goto fail;
l247: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l246;
      }
      goto fail;
l248: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l247;
      }
      goto fail;
l249: if (!readnext())
      {
         goto fail;
      }
      if (((121 <= next) && (next <= 121)))
      {
         goto l248;
      }
      goto fail;
l250: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l250;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l251: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x64,8);
         append(&b2,0x6f,8);
         append(&b2,0x63,8);
         append(&b2,0x5f,8);
         append(&b2,0x69,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x64,8);
         append(&b5,0x6f,8);
         append(&b5,0x63,8);
         append(&b5,0x5f,8);
         append(&b5,0x69,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x64,8);
         append(&b9,0x6f,8);
         append(&b9,0x63,8);
         append(&b9,0x5f,8);
         append(&b9,0x69,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x64,8);
         append(&b12,0x6f,8);
         append(&b12,0x63,8);
         append(&b12,0x5f,8);
         append(&b12,0x69,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l250;
      }
      goto fail;
l252: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l251;
      }
      goto fail;
l253: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l252;
      }
      goto fail;
l254: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l253;
      }
      goto fail;
l255: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l254;
      }
      goto fail;
l256: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l255;
      }
      goto fail;
l257: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l256;
      }
      goto fail;
l258: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l257;
      }
      goto fail;
l259: if (!readnext())
      {
         goto fail;
      }
      if (((44 <= next) && (next <= 44)))
      {
         concat(&b1,&b2);
         append(&b1,0x2c,8);
         concat(&b8,&b5);
         append(&b8,0x2c,8);
         goto l2;
      }
      if (((48 <= next) && (next <= 57)))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l259;
      }
      if (((125 <= next) && (next <= 125)))
      {
         concat(&b1,&b9);
         append(&b1,0x7d,8);
         concat(&b8,&b12);
         append(&b8,0x7d,8);
         goto l1;
      }
      goto fail;
l260: if (!readnext())
      {
         goto fail;
      }
      if (((48 <= next) && (next <= 57)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x70,8);
         append(&b2,0x61,8);
         append(&b2,0x67,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x70,8);
         append(&b5,0x61,8);
         append(&b5,0x67,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x70,8);
         append(&b9,0x61,8);
         append(&b9,0x67,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x70,8);
         append(&b12,0x61,8);
         append(&b12,0x67,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l259;
      }
      goto fail;
l261: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l260;
      }
      goto fail;
l262: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l261;
      }
      goto fail;
l263: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l262;
      }
      goto fail;
l264: if (!readnext())
      {
         goto fail;
      }
      if (((103 <= next) && (next <= 103)))
      {
         goto l263;
      }
      goto fail;
l265: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l264;
      }
      goto fail;
l266: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l267: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x69,8);
         append(&b2,0x6d,8);
         append(&b2,0x70,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x73,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x69,8);
         append(&b5,0x6d,8);
         append(&b5,0x70,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x73,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x69,8);
         append(&b9,0x6d,8);
         append(&b9,0x70,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x73,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x69,8);
         append(&b12,0x6d,8);
         append(&b12,0x70,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x73,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         goto l266;
      }
      goto fail;
l268: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l267;
      }
      goto fail;
l269: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l268;
      }
      goto fail;
l270: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l269;
      }
      goto fail;
l271: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l270;
      }
      goto fail;
l272: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l271;
      }
      goto fail;
l273: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l272;
      }
      goto fail;
l274: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l273;
      }
      goto fail;
l275: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l274;
      }
      goto fail;
l276: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x70,8);
         append(&b2,0x61,8);
         append(&b2,0x67,8);
         append(&b2,0x65,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x70,8);
         append(&b5,0x61,8);
         append(&b5,0x67,8);
         append(&b5,0x65,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x70,8);
         append(&b9,0x61,8);
         append(&b9,0x67,8);
         append(&b9,0x65,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x70,8);
         append(&b12,0x61,8);
         append(&b12,0x67,8);
         append(&b12,0x65,8);
         goto l266;
      }
      goto fail;
l277: if (!readnext())
      {
         goto fail;
      }
      if (((103 <= next) && (next <= 103)))
      {
         goto l276;
      }
      goto fail;
l278: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l277;
      }
      goto fail;
l279: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x6c,8);
         append(&b2,0x61,8);
         append(&b2,0x74,8);
         append(&b2,0x65,8);
         append(&b2,0x64,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x6c,8);
         append(&b5,0x61,8);
         append(&b5,0x74,8);
         append(&b5,0x65,8);
         append(&b5,0x64,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x6c,8);
         append(&b9,0x61,8);
         append(&b9,0x74,8);
         append(&b9,0x65,8);
         append(&b9,0x64,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x6c,8);
         append(&b12,0x61,8);
         append(&b12,0x74,8);
         append(&b12,0x65,8);
         append(&b12,0x64,8);
         goto l266;
      }
      goto fail;
l280: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l279;
      }
      goto fail;
l281: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l280;
      }
      goto fail;
l282: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l281;
      }
      goto fail;
l283: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l282;
      }
      goto fail;
l284: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l283;
      }
      goto fail;
l285: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x61,8);
         append(&b2,0x72,8);
         append(&b2,0x63,8);
         append(&b2,0x68,8);
         append(&b2,0x69,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x61,8);
         append(&b5,0x72,8);
         append(&b5,0x63,8);
         append(&b5,0x68,8);
         append(&b5,0x69,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x61,8);
         append(&b9,0x72,8);
         append(&b9,0x63,8);
         append(&b9,0x68,8);
         append(&b9,0x69,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x61,8);
         append(&b12,0x72,8);
         append(&b12,0x63,8);
         append(&b12,0x68,8);
         append(&b12,0x69,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         goto l266;
      }
      goto fail;
l286: if (!readnext())
      {
         goto fail;
      }
      if (((118 <= next) && (next <= 118)))
      {
         goto l285;
      }
      goto fail;
l287: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l286;
      }
      goto fail;
l288: if (!readnext())
      {
         goto fail;
      }
      if (((104 <= next) && (next <= 104)))
      {
         goto l287;
      }
      goto fail;
l289: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l288;
      }
      goto fail;
l290: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l547;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l275;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l278;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l284;
      }
      goto fail;
l291: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l290;
      }
      goto fail;
l292: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l291;
      }
      goto fail;
l293: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l292;
      }
      goto fail;
l294: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l293;
      }
      goto fail;
l295: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l294;
      }
      goto fail;
l296: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l297: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l296;
      }
      goto fail;
l298: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l297;
      }
      goto fail;
l299: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l298;
      }
      goto fail;
l300: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l299;
      }
      goto fail;
l301: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l300;
      }
      goto fail;
l302: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l301;
      }
      goto fail;
l303: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l302;
      }
      goto fail;
l304: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l303;
      }
      goto fail;
l305: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l304;
      }
      goto fail;
l306: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l305;
      }
      goto fail;
l307: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l306;
      }
      goto fail;
l308: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l307;
      }
      goto fail;
l309: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l308;
      }
      goto fail;
l310: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l309;
      }
      goto fail;
l311: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l310;
      }
      goto fail;
l312: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x6f,8);
         append(&b2,0x72,8);
         append(&b2,0x5f,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x6e,8);
         append(&b2,0x61,8);
         append(&b2,0x6d,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x6f,8);
         append(&b5,0x72,8);
         append(&b5,0x5f,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x6e,8);
         append(&b5,0x61,8);
         append(&b5,0x6d,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x6f,8);
         append(&b9,0x72,8);
         append(&b9,0x5f,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x6e,8);
         append(&b9,0x61,8);
         append(&b9,0x6d,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x6f,8);
         append(&b12,0x72,8);
         append(&b12,0x5f,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x6e,8);
         append(&b12,0x61,8);
         append(&b12,0x6d,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l311;
      }
      goto fail;
l313: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x6f,8);
         append(&b2,0x72,8);
         append(&b2,0x5f,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x6e,8);
         append(&b2,0x61,8);
         append(&b2,0x6d,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x6e,8);
         append(&b2,0x75,8);
         append(&b2,0x6c,8);
         append(&b2,0x6c,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x6f,8);
         append(&b5,0x72,8);
         append(&b5,0x5f,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x6e,8);
         append(&b5,0x61,8);
         append(&b5,0x6d,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x6e,8);
         append(&b5,0x75,8);
         append(&b5,0x6c,8);
         append(&b5,0x6c,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x6f,8);
         append(&b9,0x72,8);
         append(&b9,0x5f,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x6e,8);
         append(&b9,0x61,8);
         append(&b9,0x6d,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x6e,8);
         append(&b9,0x75,8);
         append(&b9,0x6c,8);
         append(&b9,0x6c,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x6f,8);
         append(&b12,0x72,8);
         append(&b12,0x5f,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x6e,8);
         append(&b12,0x61,8);
         append(&b12,0x6d,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x6e,8);
         append(&b12,0x75,8);
         append(&b12,0x6c,8);
         append(&b12,0x6c,8);
         goto l4;
      }
      goto fail;
l314: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l313;
      }
      goto fail;
l315: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l314;
      }
      goto fail;
l316: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l312;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l315;
      }
      goto fail;
l317: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l316;
      }
      goto fail;
l318: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l317;
      }
      goto fail;
l319: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l318;
      }
      goto fail;
l320: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l319;
      }
      goto fail;
l321: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l320;
      }
      goto fail;
l322: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l323: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l322;
      }
      goto fail;
l324: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l323;
      }
      goto fail;
l325: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l324;
      }
      goto fail;
l326: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l325;
      }
      goto fail;
l327: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l326;
      }
      goto fail;
l328: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l327;
      }
      goto fail;
l329: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l328;
      }
      goto fail;
l330: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l329;
      }
      goto fail;
l331: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l330;
      }
      goto fail;
l332: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l331;
      }
      goto fail;
l333: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l332;
      }
      goto fail;
l334: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l333;
      }
      goto fail;
l335: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l334;
      }
      goto fail;
l336: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l335;
      }
      goto fail;
l337: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l336;
      }
      goto fail;
l338: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         append(&b2,0x69,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         append(&b5,0x69,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         append(&b9,0x69,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         append(&b12,0x69,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l337;
      }
      goto fail;
l339: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l338;
      }
      goto fail;
l340: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l339;
      }
      goto fail;
l341: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l340;
      }
      goto fail;
l342: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l341;
      }
      goto fail;
l343: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l342;
      }
      goto fail;
l344: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l343;
      }
      goto fail;
l345: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l346: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6d,8);
         append(&b2,0x61,8);
         append(&b2,0x69,8);
         append(&b2,0x6c,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6d,8);
         append(&b5,0x61,8);
         append(&b5,0x69,8);
         append(&b5,0x6c,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6d,8);
         append(&b9,0x61,8);
         append(&b9,0x69,8);
         append(&b9,0x6c,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6d,8);
         append(&b12,0x61,8);
         append(&b12,0x69,8);
         append(&b12,0x6c,8);
         goto l345;
      }
      goto fail;
l347: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l346;
      }
      goto fail;
l348: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l347;
      }
      goto fail;
l349: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l348;
      }
      goto fail;
l350: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x66,8);
         append(&b2,0x61,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x62,8);
         append(&b2,0x6f,8);
         append(&b2,0x6f,8);
         append(&b2,0x6b,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x66,8);
         append(&b5,0x61,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x62,8);
         append(&b5,0x6f,8);
         append(&b5,0x6f,8);
         append(&b5,0x6b,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x66,8);
         append(&b9,0x61,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x62,8);
         append(&b9,0x6f,8);
         append(&b9,0x6f,8);
         append(&b9,0x6b,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x66,8);
         append(&b12,0x61,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x62,8);
         append(&b12,0x6f,8);
         append(&b12,0x6f,8);
         append(&b12,0x6b,8);
         goto l345;
      }
      goto fail;
l351: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l350;
      }
      goto fail;
l352: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l351;
      }
      goto fail;
l353: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l352;
      }
      goto fail;
l354: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l353;
      }
      goto fail;
l355: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l354;
      }
      goto fail;
l356: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l355;
      }
      goto fail;
l357: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x74,8);
         append(&b2,0x77,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x74,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x74,8);
         append(&b5,0x77,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x74,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x74,8);
         append(&b9,0x77,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x74,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x74,8);
         append(&b12,0x77,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x74,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         goto l345;
      }
      goto fail;
l358: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l357;
      }
      goto fail;
l359: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l358;
      }
      goto fail;
l360: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l359;
      }
      goto fail;
l361: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l360;
      }
      goto fail;
l362: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x67,8);
         append(&b2,0x6f,8);
         append(&b2,0x6f,8);
         append(&b2,0x67,8);
         append(&b2,0x6c,8);
         append(&b2,0x65,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x67,8);
         append(&b5,0x6f,8);
         append(&b5,0x6f,8);
         append(&b5,0x67,8);
         append(&b5,0x6c,8);
         append(&b5,0x65,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x67,8);
         append(&b9,0x6f,8);
         append(&b9,0x6f,8);
         append(&b9,0x67,8);
         append(&b9,0x6c,8);
         append(&b9,0x65,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x67,8);
         append(&b12,0x6f,8);
         append(&b12,0x6f,8);
         append(&b12,0x67,8);
         append(&b12,0x6c,8);
         append(&b12,0x65,8);
         goto l345;
      }
      goto fail;
l363: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l362;
      }
      goto fail;
l364: if (!readnext())
      {
         goto fail;
      }
      if (((103 <= next) && (next <= 103)))
      {
         goto l363;
      }
      goto fail;
l365: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l364;
      }
      goto fail;
l366: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l365;
      }
      goto fail;
l367: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x74,8);
         append(&b2,0x75,8);
         append(&b2,0x6d,8);
         append(&b2,0x62,8);
         append(&b2,0x6c,8);
         append(&b2,0x72,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x74,8);
         append(&b5,0x75,8);
         append(&b5,0x6d,8);
         append(&b5,0x62,8);
         append(&b5,0x6c,8);
         append(&b5,0x72,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x74,8);
         append(&b9,0x75,8);
         append(&b9,0x6d,8);
         append(&b9,0x62,8);
         append(&b9,0x6c,8);
         append(&b9,0x72,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x74,8);
         append(&b12,0x75,8);
         append(&b12,0x6d,8);
         append(&b12,0x62,8);
         append(&b12,0x6c,8);
         append(&b12,0x72,8);
         goto l345;
      }
      goto fail;
l368: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l367;
      }
      goto fail;
l369: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l368;
      }
      goto fail;
l370: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l369;
      }
      goto fail;
l371: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x76,8);
         append(&b2,0x69,8);
         append(&b2,0x63,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x6c,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x6b,8);
         append(&b2,0x65,8);
         append(&b2,0x64,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x76,8);
         append(&b5,0x69,8);
         append(&b5,0x63,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x6c,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x6b,8);
         append(&b5,0x65,8);
         append(&b5,0x64,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x76,8);
         append(&b9,0x69,8);
         append(&b9,0x63,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x6c,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x6b,8);
         append(&b9,0x65,8);
         append(&b9,0x64,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x76,8);
         append(&b12,0x69,8);
         append(&b12,0x63,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x6c,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x6b,8);
         append(&b12,0x65,8);
         append(&b12,0x64,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         goto l345;
      }
      goto fail;
l372: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l371;
      }
      goto fail;
l373: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l372;
      }
      goto fail;
l374: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l373;
      }
      goto fail;
l375: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         goto l374;
      }
      goto fail;
l376: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l375;
      }
      goto fail;
l377: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l376;
      }
      goto fail;
l378: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l349;
      }
      if (((102 <= next) && (next <= 102)))
      {
         goto l356;
      }
      if (((103 <= next) && (next <= 103)))
      {
         goto l366;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l377;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l576;
      }
      goto fail;
l379: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l378;
      }
      goto fail;
l380: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l379;
      }
      goto fail;
l381: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l380;
      }
      goto fail;
l382: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l381;
      }
      goto fail;
l383: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l382;
      }
      goto fail;
l384: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l383;
      }
      goto fail;
l385: if (!readnext())
      {
         goto fail;
      }
      if (((118 <= next) && (next <= 118)))
      {
         goto l384;
      }
      goto fail;
l386: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l385;
      }
      goto fail;
l387: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l386;
      }
      goto fail;
l388: if (!readnext())
      {
         goto fail;
      }
      if (((44 <= next) && (next <= 44)))
      {
         concat(&b1,&b2);
         append(&b1,0x2c,8);
         concat(&b8,&b5);
         append(&b8,0x2c,8);
         goto l2;
      }
      if (((48 <= next) && (next <= 57)))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l388;
      }
      if (((125 <= next) && (next <= 125)))
      {
         concat(&b1,&b9);
         append(&b1,0x7d,8);
         concat(&b8,&b12);
         append(&b8,0x7d,8);
         goto l1;
      }
      goto fail;
l389: if (!readnext())
      {
         goto fail;
      }
      if (((48 <= next) && (next <= 57)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x64,8);
         append(&b2,0x65,8);
         append(&b2,0x78,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x64,8);
         append(&b5,0x65,8);
         append(&b5,0x78,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x64,8);
         append(&b9,0x65,8);
         append(&b9,0x78,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x64,8);
         append(&b12,0x65,8);
         append(&b12,0x78,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l388;
      }
      goto fail;
l390: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l389;
      }
      goto fail;
l391: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l390;
      }
      goto fail;
l392: if (!readnext())
      {
         goto fail;
      }
      if (((120 <= next) && (next <= 120)))
      {
         goto l391;
      }
      goto fail;
l393: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l392;
      }
      goto fail;
l394: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l393;
      }
      goto fail;
l395: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l394;
      }
      goto fail;
l396: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l396;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l397: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x66,8);
         append(&b2,0x6f,8);
         append(&b2,0x62,8);
         append(&b2,0x6f,8);
         append(&b2,0x78,8);
         append(&b2,0x69,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x66,8);
         append(&b5,0x6f,8);
         append(&b5,0x62,8);
         append(&b5,0x6f,8);
         append(&b5,0x78,8);
         append(&b5,0x69,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x66,8);
         append(&b9,0x6f,8);
         append(&b9,0x62,8);
         append(&b9,0x6f,8);
         append(&b9,0x78,8);
         append(&b9,0x69,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x66,8);
         append(&b12,0x6f,8);
         append(&b12,0x62,8);
         append(&b12,0x6f,8);
         append(&b12,0x78,8);
         append(&b12,0x69,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l396;
      }
      goto fail;
l398: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l397;
      }
      goto fail;
l399: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l398;
      }
      goto fail;
l400: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l399;
      }
      goto fail;
l401: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l400;
      }
      goto fail;
l402: if (!readnext())
      {
         goto fail;
      }
      if (((120 <= next) && (next <= 120)))
      {
         goto l401;
      }
      goto fail;
l403: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l402;
      }
      goto fail;
l404: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l403;
      }
      goto fail;
l405: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l404;
      }
      goto fail;
l406: if (!readnext())
      {
         goto fail;
      }
      if (((102 <= next) && (next <= 102)))
      {
         goto l405;
      }
      goto fail;
l407: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l406;
      }
      goto fail;
l408: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l408;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l409: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x75,8);
         append(&b2,0x72,8);
         append(&b2,0x6c,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x75,8);
         append(&b5,0x72,8);
         append(&b5,0x6c,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x75,8);
         append(&b9,0x72,8);
         append(&b9,0x6c,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x75,8);
         append(&b12,0x72,8);
         append(&b12,0x6c,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l408;
      }
      goto fail;
l410: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l409;
      }
      goto fail;
l411: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l410;
      }
      goto fail;
l412: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l411;
      }
      goto fail;
l413: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l412;
      }
      goto fail;
l414: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l414;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l415: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x73,8);
         append(&b2,0x75,8);
         append(&b2,0x62,8);
         append(&b2,0x6a,8);
         append(&b2,0x65,8);
         append(&b2,0x63,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x6c,8);
         append(&b2,0x69,8);
         append(&b2,0x6e,8);
         append(&b2,0x6b,8);
         append(&b2,0x5f,8);
         append(&b2,0x70,8);
         append(&b2,0x6f,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x69,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x73,8);
         append(&b5,0x75,8);
         append(&b5,0x62,8);
         append(&b5,0x6a,8);
         append(&b5,0x65,8);
         append(&b5,0x63,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x6c,8);
         append(&b5,0x69,8);
         append(&b5,0x6e,8);
         append(&b5,0x6b,8);
         append(&b5,0x5f,8);
         append(&b5,0x70,8);
         append(&b5,0x6f,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x69,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x73,8);
         append(&b9,0x75,8);
         append(&b9,0x62,8);
         append(&b9,0x6a,8);
         append(&b9,0x65,8);
         append(&b9,0x63,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x6c,8);
         append(&b9,0x69,8);
         append(&b9,0x6e,8);
         append(&b9,0x6b,8);
         append(&b9,0x5f,8);
         append(&b9,0x70,8);
         append(&b9,0x6f,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x69,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x73,8);
         append(&b12,0x75,8);
         append(&b12,0x62,8);
         append(&b12,0x6a,8);
         append(&b12,0x65,8);
         append(&b12,0x63,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x6c,8);
         append(&b12,0x69,8);
         append(&b12,0x6e,8);
         append(&b12,0x6b,8);
         append(&b12,0x5f,8);
         append(&b12,0x70,8);
         append(&b12,0x6f,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x69,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l414;
      }
      goto fail;
l416: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l415;
      }
      goto fail;
l417: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l416;
      }
      goto fail;
l418: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l417;
      }
      goto fail;
l419: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l418;
      }
      goto fail;
l420: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l419;
      }
      goto fail;
l421: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l420;
      }
      goto fail;
l422: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l421;
      }
      goto fail;
l423: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l422;
      }
      goto fail;
l424: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l423;
      }
      goto fail;
l425: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l424;
      }
      goto fail;
l426: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l425;
      }
      goto fail;
l427: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         goto l426;
      }
      goto fail;
l428: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l427;
      }
      goto fail;
l429: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l428;
      }
      goto fail;
l430: if (!readnext())
      {
         goto fail;
      }
      if (((44 <= next) && (next <= 44)))
      {
         concat(&b1,&b2);
         append(&b1,0x2c,8);
         concat(&b8,&b5);
         append(&b8,0x2c,8);
         goto l2;
      }
      if (((48 <= next) && (next <= 57)))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l430;
      }
      if (((125 <= next) && (next <= 125)))
      {
         concat(&b1,&b9);
         append(&b1,0x7d,8);
         concat(&b8,&b12);
         append(&b8,0x7d,8);
         goto l1;
      }
      goto fail;
l431: if (!readnext())
      {
         goto fail;
      }
      if (((48 <= next) && (next <= 57)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x70,8);
         append(&b2,0x6f,8);
         append(&b2,0x73,8);
         append(&b2,0x69,8);
         append(&b2,0x74,8);
         append(&b2,0x69,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x70,8);
         append(&b5,0x6f,8);
         append(&b5,0x73,8);
         append(&b5,0x69,8);
         append(&b5,0x74,8);
         append(&b5,0x69,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x70,8);
         append(&b9,0x6f,8);
         append(&b9,0x73,8);
         append(&b9,0x69,8);
         append(&b9,0x74,8);
         append(&b9,0x69,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x70,8);
         append(&b12,0x6f,8);
         append(&b12,0x73,8);
         append(&b12,0x69,8);
         append(&b12,0x74,8);
         append(&b12,0x69,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l430;
      }
      goto fail;
l432: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l431;
      }
      goto fail;
l433: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l432;
      }
      goto fail;
l434: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l433;
      }
      goto fail;
l435: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l434;
      }
      goto fail;
l436: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l435;
      }
      goto fail;
l437: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l436;
      }
      goto fail;
l438: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l437;
      }
      goto fail;
l439: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l438;
      }
      goto fail;
l440: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l439;
      }
      goto fail;
l441: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l442: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l441;
      }
      goto fail;
l443: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l442;
      }
      goto fail;
l444: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l443;
      }
      goto fail;
l445: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l444;
      }
      goto fail;
l446: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l445;
      }
      goto fail;
l447: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l446;
      }
      goto fail;
l448: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l447;
      }
      goto fail;
l449: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l448;
      }
      goto fail;
l450: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l449;
      }
      goto fail;
l451: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l450;
      }
      goto fail;
l452: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l451;
      }
      goto fail;
l453: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l452;
      }
      goto fail;
l454: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l453;
      }
      goto fail;
l455: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l454;
      }
      goto fail;
l456: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l455;
      }
      goto fail;
l457: if (!readnext())
      {
         goto fail;
      }
      if ((((48 <= next) && (next <= 57)) || ((97 <= next) && (next <= 102))))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         append(&b2,0x69,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         append(&b5,0x69,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         append(&b9,0x69,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         append(&b12,0x69,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l456;
      }
      goto fail;
l458: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l457;
      }
      goto fail;
l459: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l458;
      }
      goto fail;
l460: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l459;
      }
      goto fail;
l461: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l460;
      }
      goto fail;
l462: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l461;
      }
      goto fail;
l463: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l462;
      }
      goto fail;
l464: if (!readnext())
      {
         goto fail;
      }
      if (((44 <= next) && (next <= 44)))
      {
         concat(&b1,&b2);
         append(&b1,0x2c,8);
         concat(&b8,&b5);
         append(&b8,0x2c,8);
         goto l2;
      }
      if (((48 <= next) && (next <= 57)))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l464;
      }
      if (((125 <= next) && (next <= 125)))
      {
         concat(&b1,&b9);
         append(&b1,0x7d,8);
         concat(&b8,&b12);
         append(&b8,0x7d,8);
         goto l1;
      }
      goto fail;
l465: if (!readnext())
      {
         goto fail;
      }
      if (((48 <= next) && (next <= 57)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x65,8);
         append(&b2,0x6d,8);
         append(&b2,0x62,8);
         append(&b2,0x65,8);
         append(&b2,0x64,8);
         append(&b2,0x69,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x65,8);
         append(&b5,0x6d,8);
         append(&b5,0x62,8);
         append(&b5,0x65,8);
         append(&b5,0x64,8);
         append(&b5,0x69,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x65,8);
         append(&b9,0x6d,8);
         append(&b9,0x62,8);
         append(&b9,0x65,8);
         append(&b9,0x64,8);
         append(&b9,0x69,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x65,8);
         append(&b12,0x6d,8);
         append(&b12,0x62,8);
         append(&b12,0x65,8);
         append(&b12,0x64,8);
         append(&b12,0x69,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l464;
      }
      goto fail;
l466: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l465;
      }
      goto fail;
l467: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l466;
      }
      goto fail;
l468: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l467;
      }
      goto fail;
l469: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l468;
      }
      goto fail;
l470: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l469;
      }
      goto fail;
l471: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l470;
      }
      goto fail;
l472: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l471;
      }
      goto fail;
l473: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l472;
      }
      goto fail;
l474: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l474;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l475: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x6f,8);
         append(&b2,0x6b,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x6f,8);
         append(&b5,0x6b,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x6f,8);
         append(&b9,0x6b,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x6f,8);
         append(&b12,0x6b,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l474;
      }
      goto fail;
l476: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l475;
      }
      goto fail;
l477: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l476;
      }
      goto fail;
l478: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l477;
      }
      goto fail;
l479: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l478;
      }
      goto fail;
l480: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         goto l479;
      }
      goto fail;
l481: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l481;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l482: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x62,8);
         append(&b2,0x75,8);
         append(&b2,0x69,8);
         append(&b2,0x6c,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x62,8);
         append(&b5,0x75,8);
         append(&b5,0x69,8);
         append(&b5,0x6c,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x62,8);
         append(&b9,0x75,8);
         append(&b9,0x69,8);
         append(&b9,0x6c,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x62,8);
         append(&b12,0x75,8);
         append(&b12,0x69,8);
         append(&b12,0x6c,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l481;
      }
      goto fail;
l483: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l482;
      }
      goto fail;
l484: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l483;
      }
      goto fail;
l485: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l484;
      }
      goto fail;
l486: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l485;
      }
      goto fail;
l487: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l486;
      }
      goto fail;
l488: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l487;
      }
      goto fail;
l489: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l489;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l490: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x6e,8);
         append(&b2,0x61,8);
         append(&b2,0x6d,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x6e,8);
         append(&b5,0x61,8);
         append(&b5,0x6d,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x6e,8);
         append(&b9,0x61,8);
         append(&b9,0x6d,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x6e,8);
         append(&b12,0x61,8);
         append(&b12,0x6d,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l489;
      }
      goto fail;
l491: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l490;
      }
      goto fail;
l492: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l491;
      }
      goto fail;
l493: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l492;
      }
      goto fail;
l494: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l493;
      }
      goto fail;
l495: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l494;
      }
      goto fail;
l496: if (!readnext())
      {
         goto fail;
      }
      if ((((0 <= next) && (next <= 33)) || ((35 <= next) && (next <= 255))))
      {
         append(&b2,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b5,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b9,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b12,((uint8_t)(tbl[0][next])) << 0,8);
         goto l496;
      }
      if (((34 <= next) && (next <= 34)))
      {
         append(&b2,0x22,8);
         append(&b5,0x22,8);
         append(&b9,0x22,8);
         append(&b12,0x22,8);
         goto l4;
      }
      goto fail;
l497: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x76,8);
         append(&b2,0x5f,8);
         append(&b2,0x63,8);
         append(&b2,0x6f,8);
         append(&b2,0x6d,8);
         append(&b2,0x70,8);
         append(&b2,0x6f,8);
         append(&b2,0x6e,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x76,8);
         append(&b5,0x5f,8);
         append(&b5,0x63,8);
         append(&b5,0x6f,8);
         append(&b5,0x6d,8);
         append(&b5,0x70,8);
         append(&b5,0x6f,8);
         append(&b5,0x6e,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x76,8);
         append(&b9,0x5f,8);
         append(&b9,0x63,8);
         append(&b9,0x6f,8);
         append(&b9,0x6d,8);
         append(&b9,0x70,8);
         append(&b9,0x6f,8);
         append(&b9,0x6e,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x76,8);
         append(&b12,0x5f,8);
         append(&b12,0x63,8);
         append(&b12,0x6f,8);
         append(&b12,0x6d,8);
         append(&b12,0x70,8);
         append(&b12,0x6f,8);
         append(&b12,0x6e,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         goto l496;
      }
      goto fail;
l498: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l497;
      }
      goto fail;
l499: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l498;
      }
      goto fail;
l500: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l499;
      }
      goto fail;
l501: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l500;
      }
      goto fail;
l502: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l501;
      }
      goto fail;
l503: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l502;
      }
      goto fail;
l504: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l503;
      }
      goto fail;
l505: if (!readnext())
      {
         goto fail;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l504;
      }
      goto fail;
l506: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l505;
      }
      goto fail;
l507: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l506;
      }
      goto fail;
l508: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l554;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l524;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l546;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l9;
      }
      if (((118 <= next) && (next <= 118)))
      {
         goto l517;
      }
      goto fail;
l509: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l520;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l31;
      }
      goto fail;
l510: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l118;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l77;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l106;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l145;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l55;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l509;
      }
      goto fail;
l511: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l510;
      }
      goto fail;
l512: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l511;
      }
      goto fail;
l513: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l512;
      }
      goto fail;
l514: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l513;
      }
      goto fail;
l515: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l514;
      }
      goto fail;
l516: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l515;
      }
      goto fail;
l517: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l516;
      }
      goto fail;
l518: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l85;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l321;
      }
      goto fail;
l519: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l518;
      }
      goto fail;
l520: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l519;
      }
      goto fail;
l521: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l344;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l488;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l507;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l178;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l495;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l564;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l169;
      }
      goto fail;
l522: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l521;
      }
      goto fail;
l523: if (!readnext())
      {
         goto fail;
      }
      if (((118 <= next) && (next <= 118)))
      {
         goto l522;
      }
      goto fail;
l524: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l523;
      }
      if (((118 <= next) && (next <= 118)))
      {
         goto l538;
      }
      goto fail;
l525: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x65,8);
         append(&b2,0x76,8);
         append(&b2,0x65,8);
         append(&b2,0x6e,8);
         append(&b2,0x74,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x70,8);
         append(&b2,0x61,8);
         append(&b2,0x67,8);
         append(&b2,0x65,8);
         append(&b2,0x72,8);
         append(&b2,0x65,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         append(&b2,0x22,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x65,8);
         append(&b5,0x76,8);
         append(&b5,0x65,8);
         append(&b5,0x6e,8);
         append(&b5,0x74,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x70,8);
         append(&b5,0x61,8);
         append(&b5,0x67,8);
         append(&b5,0x65,8);
         append(&b5,0x72,8);
         append(&b5,0x65,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         append(&b5,0x22,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x65,8);
         append(&b9,0x76,8);
         append(&b9,0x65,8);
         append(&b9,0x6e,8);
         append(&b9,0x74,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x70,8);
         append(&b9,0x61,8);
         append(&b9,0x67,8);
         append(&b9,0x65,8);
         append(&b9,0x72,8);
         append(&b9,0x65,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         append(&b9,0x22,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x65,8);
         append(&b12,0x76,8);
         append(&b12,0x65,8);
         append(&b12,0x6e,8);
         append(&b12,0x74,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x70,8);
         append(&b12,0x61,8);
         append(&b12,0x67,8);
         append(&b12,0x65,8);
         append(&b12,0x72,8);
         append(&b12,0x65,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         append(&b12,0x22,8);
         goto l4;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l208;
      }
      goto fail;
l526: if (!readnext())
      {
         goto fail;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l191;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l223;
      }
      goto fail;
l527: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l525;
      }
      goto fail;
l528: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l527;
      }
      goto fail;
l529: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l528;
      }
      goto fail;
l530: if (!readnext())
      {
         goto fail;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l529;
      }
      goto fail;
l531: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l530;
      }
      goto fail;
l532: if (!readnext())
      {
         goto fail;
      }
      if (((103 <= next) && (next <= 103)))
      {
         goto l531;
      }
      goto fail;
l533: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l532;
      }
      goto fail;
l534: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l395;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l575;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l387;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l230;
      }
      goto fail;
l535: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l534;
      }
      goto fail;
l536: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l535;
      }
      goto fail;
l537: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l536;
      }
      goto fail;
l538: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l537;
      }
      goto fail;
l539: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l258;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l407;
      }
      if (((108 <= next) && (next <= 108)))
      {
         goto l429;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l265;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l249;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l413;
      }
      goto fail;
l540: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l539;
      }
      goto fail;
l541: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l540;
      }
      goto fail;
l542: if (!readnext())
      {
         goto fail;
      }
      if (((99 <= next) && (next <= 99)))
      {
         goto l541;
      }
      goto fail;
l543: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l542;
      }
      goto fail;
l544: if (!readnext())
      {
         goto fail;
      }
      if (((106 <= next) && (next <= 106)))
      {
         goto l543;
      }
      goto fail;
l545: if (!readnext())
      {
         goto fail;
      }
      if (((98 <= next) && (next <= 98)))
      {
         goto l544;
      }
      goto fail;
l546: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l545;
      }
      goto fail;
l547: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         reset(&b2);
         append(&b2,0x22,8);
         append(&b2,0x63,8);
         append(&b2,0x61,8);
         append(&b2,0x75,8);
         append(&b2,0x73,8);
         append(&b2,0x65,8);
         append(&b2,0x5f,8);
         append(&b2,0x74,8);
         append(&b2,0x79,8);
         append(&b2,0x70,8);
         append(&b2,0x65,8);
         append(&b2,0x22,8);
         append(&b2,0x3a,8);
         append(&b2,0x22,8);
         append(&b2,0x61,8);
         append(&b2,0x64,8);
         reset(&b5);
         append(&b5,0x22,8);
         append(&b5,0x63,8);
         append(&b5,0x61,8);
         append(&b5,0x75,8);
         append(&b5,0x73,8);
         append(&b5,0x65,8);
         append(&b5,0x5f,8);
         append(&b5,0x74,8);
         append(&b5,0x79,8);
         append(&b5,0x70,8);
         append(&b5,0x65,8);
         append(&b5,0x22,8);
         append(&b5,0x3a,8);
         append(&b5,0x22,8);
         append(&b5,0x61,8);
         append(&b5,0x64,8);
         reset(&b9);
         append(&b9,0x22,8);
         append(&b9,0x63,8);
         append(&b9,0x61,8);
         append(&b9,0x75,8);
         append(&b9,0x73,8);
         append(&b9,0x65,8);
         append(&b9,0x5f,8);
         append(&b9,0x74,8);
         append(&b9,0x79,8);
         append(&b9,0x70,8);
         append(&b9,0x65,8);
         append(&b9,0x22,8);
         append(&b9,0x3a,8);
         append(&b9,0x22,8);
         append(&b9,0x61,8);
         append(&b9,0x64,8);
         reset(&b12);
         append(&b12,0x22,8);
         append(&b12,0x63,8);
         append(&b12,0x61,8);
         append(&b12,0x75,8);
         append(&b12,0x73,8);
         append(&b12,0x65,8);
         append(&b12,0x5f,8);
         append(&b12,0x74,8);
         append(&b12,0x79,8);
         append(&b12,0x70,8);
         append(&b12,0x65,8);
         append(&b12,0x22,8);
         append(&b12,0x3a,8);
         append(&b12,0x22,8);
         append(&b12,0x61,8);
         append(&b12,0x64,8);
         goto l266;
      }
      if (((114 <= next) && (next <= 114)))
      {
         goto l289;
      }
      goto fail;
l548: if (!readnext())
      {
         goto fail;
      }
      if (((111 <= next) && (next <= 111)))
      {
         goto l480;
      }
      if (((121 <= next) && (next <= 121)))
      {
         goto l295;
      }
      goto fail;
l549: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l463;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l473;
      }
      if (((112 <= next) && (next <= 112)))
      {
         goto l440;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l548;
      }
      goto fail;
l550: if (!readnext())
      {
         goto fail;
      }
      if (((95 <= next) && (next <= 95)))
      {
         goto l549;
      }
      goto fail;
l551: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l550;
      }
      goto fail;
l552: if (!readnext())
      {
         goto fail;
      }
      if (((115 <= next) && (next <= 115)))
      {
         goto l551;
      }
      goto fail;
l553: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l552;
      }
      goto fail;
l554: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l553;
      }
      goto fail;
l555: if (!readnext())
      {
         goto fail;
      }
      if (((44 <= next) && (next <= 44)))
      {
         concat(&b1,&b3);
         append(&b1,0x2c,8);
         concat(&b8,&b4);
         append(&b8,0x2c,8);
         goto l2;
      }
      if (((48 <= next) && (next <= 57)))
      {
         append(&b3,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b4,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b6,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b7,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b10,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b11,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b13,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b14,((uint8_t)(tbl[0][next])) << 0,8);
         goto l555;
      }
      if (((125 <= next) && (next <= 125)))
      {
         concat(&b1,&b6);
         append(&b1,0x7d,8);
         concat(&b8,&b7);
         append(&b8,0x7d,8);
         goto l1;
      }
      goto fail;
l556: if (!readnext())
      {
         goto fail;
      }
      if (((48 <= next) && (next <= 57)))
      {
         reset(&b3);
         append(&b3,0x22,8);
         append(&b3,0x65,8);
         append(&b3,0x6e,8);
         append(&b3,0x76,8);
         append(&b3,0x5f,8);
         append(&b3,0x72,8);
         append(&b3,0x61,8);
         append(&b3,0x6e,8);
         append(&b3,0x6b,8);
         append(&b3,0x69,8);
         append(&b3,0x6e,8);
         append(&b3,0x67,8);
         append(&b3,0x22,8);
         append(&b3,0x3a,8);
         append(&b3,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b4);
         append(&b4,0x22,8);
         append(&b4,0x65,8);
         append(&b4,0x6e,8);
         append(&b4,0x76,8);
         append(&b4,0x5f,8);
         append(&b4,0x72,8);
         append(&b4,0x61,8);
         append(&b4,0x6e,8);
         append(&b4,0x6b,8);
         append(&b4,0x69,8);
         append(&b4,0x6e,8);
         append(&b4,0x67,8);
         append(&b4,0x22,8);
         append(&b4,0x3a,8);
         append(&b4,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b6);
         append(&b6,0x22,8);
         append(&b6,0x65,8);
         append(&b6,0x6e,8);
         append(&b6,0x76,8);
         append(&b6,0x5f,8);
         append(&b6,0x72,8);
         append(&b6,0x61,8);
         append(&b6,0x6e,8);
         append(&b6,0x6b,8);
         append(&b6,0x69,8);
         append(&b6,0x6e,8);
         append(&b6,0x67,8);
         append(&b6,0x22,8);
         append(&b6,0x3a,8);
         append(&b6,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b7);
         append(&b7,0x22,8);
         append(&b7,0x65,8);
         append(&b7,0x6e,8);
         append(&b7,0x76,8);
         append(&b7,0x5f,8);
         append(&b7,0x72,8);
         append(&b7,0x61,8);
         append(&b7,0x6e,8);
         append(&b7,0x6b,8);
         append(&b7,0x69,8);
         append(&b7,0x6e,8);
         append(&b7,0x67,8);
         append(&b7,0x22,8);
         append(&b7,0x3a,8);
         append(&b7,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b10);
         append(&b10,0x22,8);
         append(&b10,0x65,8);
         append(&b10,0x6e,8);
         append(&b10,0x76,8);
         append(&b10,0x5f,8);
         append(&b10,0x72,8);
         append(&b10,0x61,8);
         append(&b10,0x6e,8);
         append(&b10,0x6b,8);
         append(&b10,0x69,8);
         append(&b10,0x6e,8);
         append(&b10,0x67,8);
         append(&b10,0x22,8);
         append(&b10,0x3a,8);
         append(&b10,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b11);
         append(&b11,0x22,8);
         append(&b11,0x65,8);
         append(&b11,0x6e,8);
         append(&b11,0x76,8);
         append(&b11,0x5f,8);
         append(&b11,0x72,8);
         append(&b11,0x61,8);
         append(&b11,0x6e,8);
         append(&b11,0x6b,8);
         append(&b11,0x69,8);
         append(&b11,0x6e,8);
         append(&b11,0x67,8);
         append(&b11,0x22,8);
         append(&b11,0x3a,8);
         append(&b11,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b13);
         append(&b13,0x22,8);
         append(&b13,0x65,8);
         append(&b13,0x6e,8);
         append(&b13,0x76,8);
         append(&b13,0x5f,8);
         append(&b13,0x72,8);
         append(&b13,0x61,8);
         append(&b13,0x6e,8);
         append(&b13,0x6b,8);
         append(&b13,0x69,8);
         append(&b13,0x6e,8);
         append(&b13,0x67,8);
         append(&b13,0x22,8);
         append(&b13,0x3a,8);
         append(&b13,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b14);
         append(&b14,0x22,8);
         append(&b14,0x65,8);
         append(&b14,0x6e,8);
         append(&b14,0x76,8);
         append(&b14,0x5f,8);
         append(&b14,0x72,8);
         append(&b14,0x61,8);
         append(&b14,0x6e,8);
         append(&b14,0x6b,8);
         append(&b14,0x69,8);
         append(&b14,0x6e,8);
         append(&b14,0x67,8);
         append(&b14,0x22,8);
         append(&b14,0x3a,8);
         append(&b14,((uint8_t)(tbl[0][next])) << 0,8);
         goto l555;
      }
      goto fail;
l557: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l556;
      }
      goto fail;
l558: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l557;
      }
      goto fail;
l559: if (!readnext())
      {
         goto fail;
      }
      if (((103 <= next) && (next <= 103)))
      {
         goto l558;
      }
      goto fail;
l560: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l559;
      }
      goto fail;
l561: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l560;
      }
      goto fail;
l562: if (!readnext())
      {
         goto fail;
      }
      if (((107 <= next) && (next <= 107)))
      {
         goto l561;
      }
      goto fail;
l563: if (!readnext())
      {
         goto fail;
      }
      if (((110 <= next) && (next <= 110)))
      {
         goto l562;
      }
      goto fail;
l564: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l563;
      }
      goto fail;
l565: if (!readnext())
      {
         goto fail;
      }
      if (((44 <= next) && (next <= 44)))
      {
         concat(&b1,&b3);
         append(&b1,0x2c,8);
         concat(&b8,&b4);
         append(&b8,0x2c,8);
         goto l2;
      }
      if (((48 <= next) && (next <= 57)))
      {
         append(&b3,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b4,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b6,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b7,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b10,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b11,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b13,((uint8_t)(tbl[0][next])) << 0,8);
         append(&b14,((uint8_t)(tbl[0][next])) << 0,8);
         goto l565;
      }
      if (((125 <= next) && (next <= 125)))
      {
         concat(&b1,&b6);
         append(&b1,0x7d,8);
         concat(&b8,&b7);
         append(&b8,0x7d,8);
         goto l1;
      }
      goto fail;
l566: if (!readnext())
      {
         goto fail;
      }
      if (((48 <= next) && (next <= 57)))
      {
         reset(&b3);
         append(&b3,0x22,8);
         append(&b3,0x65,8);
         append(&b3,0x76,8);
         append(&b3,0x65,8);
         append(&b3,0x6e,8);
         append(&b3,0x74,8);
         append(&b3,0x5f,8);
         append(&b3,0x72,8);
         append(&b3,0x65,8);
         append(&b3,0x61,8);
         append(&b3,0x64,8);
         append(&b3,0x74,8);
         append(&b3,0x69,8);
         append(&b3,0x6d,8);
         append(&b3,0x65,8);
         append(&b3,0x22,8);
         append(&b3,0x3a,8);
         append(&b3,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b4);
         append(&b4,0x22,8);
         append(&b4,0x65,8);
         append(&b4,0x76,8);
         append(&b4,0x65,8);
         append(&b4,0x6e,8);
         append(&b4,0x74,8);
         append(&b4,0x5f,8);
         append(&b4,0x72,8);
         append(&b4,0x65,8);
         append(&b4,0x61,8);
         append(&b4,0x64,8);
         append(&b4,0x74,8);
         append(&b4,0x69,8);
         append(&b4,0x6d,8);
         append(&b4,0x65,8);
         append(&b4,0x22,8);
         append(&b4,0x3a,8);
         append(&b4,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b6);
         append(&b6,0x22,8);
         append(&b6,0x65,8);
         append(&b6,0x76,8);
         append(&b6,0x65,8);
         append(&b6,0x6e,8);
         append(&b6,0x74,8);
         append(&b6,0x5f,8);
         append(&b6,0x72,8);
         append(&b6,0x65,8);
         append(&b6,0x61,8);
         append(&b6,0x64,8);
         append(&b6,0x74,8);
         append(&b6,0x69,8);
         append(&b6,0x6d,8);
         append(&b6,0x65,8);
         append(&b6,0x22,8);
         append(&b6,0x3a,8);
         append(&b6,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b7);
         append(&b7,0x22,8);
         append(&b7,0x65,8);
         append(&b7,0x76,8);
         append(&b7,0x65,8);
         append(&b7,0x6e,8);
         append(&b7,0x74,8);
         append(&b7,0x5f,8);
         append(&b7,0x72,8);
         append(&b7,0x65,8);
         append(&b7,0x61,8);
         append(&b7,0x64,8);
         append(&b7,0x74,8);
         append(&b7,0x69,8);
         append(&b7,0x6d,8);
         append(&b7,0x65,8);
         append(&b7,0x22,8);
         append(&b7,0x3a,8);
         append(&b7,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b10);
         append(&b10,0x22,8);
         append(&b10,0x65,8);
         append(&b10,0x76,8);
         append(&b10,0x65,8);
         append(&b10,0x6e,8);
         append(&b10,0x74,8);
         append(&b10,0x5f,8);
         append(&b10,0x72,8);
         append(&b10,0x65,8);
         append(&b10,0x61,8);
         append(&b10,0x64,8);
         append(&b10,0x74,8);
         append(&b10,0x69,8);
         append(&b10,0x6d,8);
         append(&b10,0x65,8);
         append(&b10,0x22,8);
         append(&b10,0x3a,8);
         append(&b10,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b11);
         append(&b11,0x22,8);
         append(&b11,0x65,8);
         append(&b11,0x76,8);
         append(&b11,0x65,8);
         append(&b11,0x6e,8);
         append(&b11,0x74,8);
         append(&b11,0x5f,8);
         append(&b11,0x72,8);
         append(&b11,0x65,8);
         append(&b11,0x61,8);
         append(&b11,0x64,8);
         append(&b11,0x74,8);
         append(&b11,0x69,8);
         append(&b11,0x6d,8);
         append(&b11,0x65,8);
         append(&b11,0x22,8);
         append(&b11,0x3a,8);
         append(&b11,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b13);
         append(&b13,0x22,8);
         append(&b13,0x65,8);
         append(&b13,0x76,8);
         append(&b13,0x65,8);
         append(&b13,0x6e,8);
         append(&b13,0x74,8);
         append(&b13,0x5f,8);
         append(&b13,0x72,8);
         append(&b13,0x65,8);
         append(&b13,0x61,8);
         append(&b13,0x64,8);
         append(&b13,0x74,8);
         append(&b13,0x69,8);
         append(&b13,0x6d,8);
         append(&b13,0x65,8);
         append(&b13,0x22,8);
         append(&b13,0x3a,8);
         append(&b13,((uint8_t)(tbl[0][next])) << 0,8);
         reset(&b14);
         append(&b14,0x22,8);
         append(&b14,0x65,8);
         append(&b14,0x76,8);
         append(&b14,0x65,8);
         append(&b14,0x6e,8);
         append(&b14,0x74,8);
         append(&b14,0x5f,8);
         append(&b14,0x72,8);
         append(&b14,0x65,8);
         append(&b14,0x61,8);
         append(&b14,0x64,8);
         append(&b14,0x74,8);
         append(&b14,0x69,8);
         append(&b14,0x6d,8);
         append(&b14,0x65,8);
         append(&b14,0x22,8);
         append(&b14,0x3a,8);
         append(&b14,((uint8_t)(tbl[0][next])) << 0,8);
         goto l565;
      }
      goto fail;
l567: if (!readnext())
      {
         goto fail;
      }
      if (((58 <= next) && (next <= 58)))
      {
         goto l566;
      }
      goto fail;
l568: if (!readnext())
      {
         goto fail;
      }
      if (((34 <= next) && (next <= 34)))
      {
         goto l567;
      }
      goto fail;
l569: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l568;
      }
      goto fail;
l570: if (!readnext())
      {
         goto fail;
      }
      if (((109 <= next) && (next <= 109)))
      {
         goto l569;
      }
      goto fail;
l571: if (!readnext())
      {
         goto fail;
      }
      if (((105 <= next) && (next <= 105)))
      {
         goto l570;
      }
      goto fail;
l572: if (!readnext())
      {
         goto fail;
      }
      if (((116 <= next) && (next <= 116)))
      {
         goto l571;
      }
      goto fail;
l573: if (!readnext())
      {
         goto fail;
      }
      if (((100 <= next) && (next <= 100)))
      {
         goto l572;
      }
      goto fail;
l574: if (!readnext())
      {
         goto fail;
      }
      if (((97 <= next) && (next <= 97)))
      {
         goto l573;
      }
      goto fail;
l575: if (!readnext())
      {
         goto fail;
      }
      if (((101 <= next) && (next <= 101)))
      {
         goto l574;
      }
      goto fail;
l576: if (!readnext())
      {
         goto fail;
      }
      if (((117 <= next) && (next <= 117)))
      {
         goto l370;
      }
      if (((119 <= next) && (next <= 119)))
      {
         goto l361;
      }
      goto fail;
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

init_buffer(&b1);
init_buffer(&b2);
init_buffer(&b3);
init_buffer(&b4);
init_buffer(&b5);
init_buffer(&b6);
init_buffer(&b7);
init_buffer(&b8);
init_buffer(&b9);
init_buffer(&b10);
init_buffer(&b11);
init_buffer(&b12);
init_buffer(&b13);
init_buffer(&b14);

  match();

  if (outbuf.bitpos % BUFFER_UNIT_BITS != 0)
    writeconst(0, BUFFER_UNIT_BITS);
  buf_flush(&outbuf);
}
