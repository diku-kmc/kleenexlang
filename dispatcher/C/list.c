//
//  list.c
//
//
//  Created by Jonas Jørgensen on 03/05/2017.
//
//
#include <stdlib.h>

#include "list.h"


void push(node** head_ref, void *new_data, size_t data_size)
{
  node* new_node = malloc(sizeof(node));

  new_node->data = malloc(data_size);
  new_node->next = (*head_ref);

  int i;
  for (i=0; i<data_size; i++)
    *(char *)(new_node->data + i) = *(char *)(new_data + i);

  (*head_ref) = new_node;
}

void append(node** head_ref, void * new_data, size_t data_size) {
  node * cur;
  node * new_node = malloc(sizeof(node));

  new_node->next = NULL;
  new_node->data = malloc(data_size);
  int i;
  for (i=0; i<data_size; i++)
    *(char *)(new_node->data + i) = *(char *)(new_data + i);

  if ((*head_ref) == NULL) {
    (*head_ref) = new_node;
    return;
  }

  cur = (*head_ref);
  while (cur->next != NULL) {
    cur = cur->next;
  }

  cur->next = new_node;
  new_node->next = NULL;

}

void pop(node** head_ref) {
  node *old = (*head_ref);
  (*head_ref) = old->next;
  free(old->data);
  free(old);
}

void printList(node *node, void (*fptr)(void *))
{
  while (node != NULL)
  {
    (*fptr)(node->data);
    node = node->next;
  }
}
