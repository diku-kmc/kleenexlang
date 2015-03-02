#include <stdio.h>
#include <stdbool.h>
#include "test.h"

int main(int argc, char *argv[])
{
  Test **current = allTests;
  bool success = true;

  setup_test();

  while (*current)
  {
    printf("    %-36s", (*current)->name);
    TestResult result;
    if (!((*current)->f(&result)))
    {
      printf("%s\n", result);
      success = false;
    }
    else
      printf("PASS.\n");

    current++;
  }

  teardown_test();

  return !success;
}
