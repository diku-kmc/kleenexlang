//
//  list.h
//
//
//  Created by Jonas Jørgensen on 03/05/2017.
//
//

#ifndef list_h
#define list_h
#include <stdlib.h>

typedef struct node {
  void * data;
  struct node *next;
} node;

void push(node** head_ref, void *new_data, size_t data_size);

void append(node** head_ref, void * new_data, size_t data_size);

void pop(node** head_ref);

void printList(node *node, void (*fptr)(void *));

#endif /* list_h */
