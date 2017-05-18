//
//  util.c
//  
//
//  Created by Jonas Jørgensen on 29/04/2017.
//
//
#include <stdlib.h>

#include "util.h"

size_t arr_size = sizeof(arr);

/**
 *    Checks if arr contains the target element.
 *    Returns 1 if target is found with in the first n elements of arr,
 *            0 otherwise.
 *
 *    arr * arr:   Array subject to search.
 *    int target: Target object of search.
 */
int /* 1 - true or 0 - false */
contains(arr *  arr, int target) {
  for (int i = 0; i < arr->size; i++) {
    if (target == arr->data[i]) {
      return 1;
    }
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
 *    arr * s: array to factor.
 *
 *    Postcondition: 
 *      - u[l[i]] = s[i]
*/
arr ** /* f[0] = lookup and f[1] = unique*/
factor(arr * s) {
  arr ** f;
  arr * u = unique(s);
  arr * l = malloc(arr_size);
  l->data = malloc(s->size * sizeof(int) + sizeof(int));
  l->size = s->size;
  
  for (int i = 0; i < s->size; i++) {
    for (int j = 0; j < u->size; j++) {
      if (u->data[j] == s->data[i]) {
        l->data[i] = j;
        break;
      }
    }
  }
  f = malloc(2 * sizeof(arr *));
  f[0] = l;
  f[1] = u;
  return f;
}


/**
 *    Produces an array representing the nested loopup t[s[i]].
 *    Returns the array g such that g[i] == t[s[i]] where i is an index of s.
 *
 *    arr * s: first array.
 *    arr * t: second array.
 *
 *    The function is only defined for instances of s, where all 
 *    values of s is a valid index of t. Diviating from this might
 *    cause undefined behaviour.
 */
arr* gather(arr * s, arr * t) {
  arr * g;
  g = malloc(sizeof(arr));
  g->data = malloc(s->size * sizeof(int));
  g->size = s->size;
  int * ga = g->data;
  int * ta = t->data;
  int * sa = t->data;
  for (int i = 0; i < s->size; i++) {
    ga[i] = ta[sa[i]];
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
 *    arr * s: target array.
 *
 *    The returned array will have the length n. 
 *    Any first index containing the value -2 indicates 
 *    the end of the sequence of unique values.
 */
arr* unique(arr * s) {
  arr * res;
  res = malloc(sizeof(arr));
  res->data = malloc(s->size * sizeof(int));
  res->size = 0;

  for (int i = 0; i < s->size; i++) {
    if (!contains(res, s->data[i])) {
      res->data[(res->size)++] = s->data[i];
    }
  }
  res->data = realloc(res->data, res->size * sizeof(int));
  return res;
}
