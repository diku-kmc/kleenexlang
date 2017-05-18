//
//  util.h
//  
//
//  Created by Jonas Jørgensen on 29/04/2017.
//
//

#ifndef util_h
#define util_h

typedef struct {
  int size;
  int * data;
} arr;

extern size_t arr_size;

arr ** factor(arr *);
arr * gather(arr *, arr *);
arr * unique(arr *);

#endif /* util_h */
