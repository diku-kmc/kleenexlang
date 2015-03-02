#ifndef TEST_H
#define TEST_H

#include <stdbool.h>

typedef char* TestResult;

typedef struct Test
{
  char *name;
  bool (*f)(TestResult *);
} Test;

extern Test *allTests[];

void setup_test();
void teardown_test();

#define SETUP void setup_test()

#define TEARDOWN void teardown_test()

#define TEST(NAME) \
  bool test_ ## NAME();\
  Test rtest_ ## NAME = { #NAME, &test_ ## NAME };\
  bool test_ ## NAME(TestResult *result)

#define TESTREF(NAME) \
  &rtest_ ## NAME

#define TESTS \
  Test *allTests[] =

#define S(x) #x
#define S_(x) S(x)
#define S__LINE__ S_(__LINE__)

#define Fail(msg) do { *result = (__FILE__ ":" S__LINE__ ": " msg); \
                       return false; } while (0)
#define Pass      do { return true; } while (0)
#define AssertCond(cond, msg)\
  do { if (!(cond)) Fail("Condition: " msg); } while (0)
#define AssertTrue(cond)\
  do { if (!(cond)) Fail("Assertion failed: " #cond); } while (0)
#define AssertFalse(cond)\
  AssertTrue(!(cond))
#define AssertNull(expr)\
  AssertCond(((expr) == NULL), "Should be null: " #expr)
#define AssertNotNull(expr)\
  AssertCond(!((expr) == NULL), "Should be non-null: " #expr)
#define AssertEqual(x,y)\
  AssertTrue((x) == (y))
#define AssertLess(x,y)\
  AssertTrue((x) < (y))
#define AssertLessEqual(x,y)\
  AssertTrue((x) <= (y))

#endif // TEST_H
