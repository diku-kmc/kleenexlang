//
//  util.c
//  
//
//  Created by Jonas Jørgensen on 29/04/2017.
//
//
#include <stdlib.h>

#include "util.h"

/**
 *    Checks if arr contains the target element.
 *    Returns 1 if target is found with in the first n elements of arr,
 *            0 otherwise.
 *
 *    int* arr:   Array subject to search.
 *    int target: Target object of search.
 *    int n:      Length of arr.
 */
int /* 1 - true or 0 - false */
contains(int*  arr, int target, int n) {
  int i;
  for (i = 0; i < n; i++) {
    if (target == arr[i]) {return 1;}
  }
  return 0;
}


/**
 *    Devides the array s into two components lookup (l) and unique (u).
 *     - unique is the set representation of s, where the order is preserved.
 *     - lookup is the mapping of indexes of s to unique. 
 *
 *    Returns an three dimensional array f, such that f[0] = lookup and f[1] = unique.
 *
 *    int* s: array to factor.
 *    int  n: lengtf of s.
 *
 *    Postcondition: 
 *      - u[l[i]] = s[i]
*/
int** /* f[0] = lookup and f[1] = unique*/
factor(int* s, int n) {
  int i, j;
  int** f;
  f = malloc(2 * sizeof(int));
  f[0] = malloc(n * sizeof(int));
  f[1] = unique(s, n);
  
  for (i = 0; i < n; i++) {
    j = 0;
    while (f[1][j] > -2) {
      if (f[1][j] == s[i]) {
        f[0][i] = j;
      }
      j++;
    }
  }
  return f;
}


/**
 *    Produces an array representing the nested loopup t[s[i]].
 *    Returns the array g such that g[i] == t[s[i]] where i is an index of s.
 *
 *    int* s: first array.
 *    int* t: second array.
 *    int  n: size of s.
 *
 *    The function is only defined for instances of s, where all 
 *    values of s is a valid index of t. Diviating from this might
 *    cause undefined behaviour.
 */
int* gather(int* s, int* t, int n) {
  int i;
  int *g;
  g =  malloc(n * sizeof(int));
  
  for (i = 0; i < n; i++) {
    g[i] = t[s[i]];
  }
  
  return g;
}


/**
 *    Creates an array contining unique values of s.
 *      - the order of which the elements occure in s 
 *        is preserved in the output.
 *
 *    Returns the array u of unique elements in s.
 *
 *    int* s: target array.
 *    int  n: length of s.
 *
 *    The returned array will have the length n. 
 *    Any first index containing the value -2 indicates 
 *    the end of the sequence of unique values.
 */
int* unique(int* s, int n) {
  int i, j=0;
  int * res;
  res = malloc(n * sizeof(int));
  for (i = 0; i < n; res[i++] = -2);
  
  for (i = 0; i < n; i++) {
    if (!contains(res, s[i], n)) {
      res[j++] = s[i];
    }
  }
//  res = realloc(res, (j+1) * sizeof(int));
  return res;
}
