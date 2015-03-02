#define BUFFER_UNIT_T uint8_t

#include "crt_header.h"
#include "test.h"

buffer_t buf1, buf2;

// Test that a buffer resizes when we write past its initial boundary using append.
TEST(ResizeTest_Append)
{
  init_buffer(&buf1);

  AssertEqual(buf1.size, INITIAL_BUFFER_SIZE);
  for (int i = 0; i < INITIAL_BUFFER_SIZE; i++)
    append(&buf1, 'a', 8);
  append(&buf1, 'b',8);
  AssertLess(INITIAL_BUFFER_SIZE, buf1.size);

  destroy_buffer(&buf1);
  return true;
}

// Test that a buffer resizes when we write past its initial boundary by
// concatenating buffers.
TEST(ResizeTest_Concat)
{
  init_buffer(&buf1);
  init_buffer(&buf2);

  if (buf1.size % 4 != 0)
  {
    Fail("Initial buffer size should be a power of 2, and at least 4.");
  }

  int testSize = (buf1.size / 4) * 3;

  for (int i = 0; i < testSize; i++)
  {
    append(&buf1, 'a', 8);
    append(&buf2, 'b', 8);
  }

  concat(&buf1, &buf2);

  // Buffer 1 should now be at least the size of its contents.
  AssertLessEqual(testSize * 2, buf1.size);

  // Check buffer contents
  for (int i = 0; i < testSize; i++)
  {
    AssertEqual(buf1.data[i], 'a');
    AssertEqual(buf1.data[testSize+i], 'b');
  }

  destroy_buffer(&buf1);
  destroy_buffer(&buf2);
  return true;  
}

SETUP
{
  outstream = fopen(__FILE__ ".out", "w+");
  init_outbuf();
}

TEARDOWN
{
  flush_outbuf();
  fclose(outstream);
  if (buf1.data != NULL)
    destroy_buffer(&buf1);
  if (buf2.data != NULL)
    destroy_buffer(&buf2);
}


TESTS
{
  TESTREF(ResizeTest_Append),
  TESTREF(ResizeTest_Concat),
  0
};
