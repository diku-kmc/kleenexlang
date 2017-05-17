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

struct node {
  void * data;
  struct node *next;
};

void push(struct node** head_ref, void *new_data, size_t data_size);

void append(struct node** head_ref, void * new_data, size_t data_size);

void pop(struct node** head_ref);

void printList(struct node *node, void (*fptr)(void *));

#endif /* list_h */
