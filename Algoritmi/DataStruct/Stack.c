#include <stdio.h>
#include <stdlib.h>

typedef struct elem {
  int value;
  struct elem *prev;
} elem;

elem *initElem();
void printElem(elem *);

elem *initElem() {
  elem *n = malloc(sizeof(elem));
  n->value = 0;
  n->prev = NULL;
  return n;
}

void printElem(elem *nd) { printf("Data: %d\n", nd->value); }

typedef struct stack {
  elem *st;
} stack;

void printStack(stack *);
stack *initStack();
void push(stack *, elem *);
int isStackEmpty(stack *);
elem *pop(stack *);
elem *top(stack *);

void printStack(stack *p) {
  elem *t;
  t = p->st;
  while (t != NULL) {
    printf("%d ", t->value);
    t = t->prev;
  }
}

stack *initStack() {
  stack *p = malloc(sizeof(stack));
  p->st = NULL;
  return p;
}

int isStackEmpty(stack *p) {
  int b = 1;
  if (p->st != NULL)
    b = 0;
  return b;
}

void push(stack *p, elem *e) {
  e->prev = p->st;
  p->st = e;
}

elem *pop(stack *p) {
  elem *t;
  t = p->st;
  p->st = t->prev;
  return t;
}

elem *top(stack *p) { return p->st; }

int main() {
  stack *p = initStack();
  int v = isStackEmpty(p);
  printf("la pila è: %B\n", v);
  for (int i = 0; i < 4; i++) {
    elem *e = initElem();
    e->value = i;
    push(p, e);
  }
  v = isStackEmpty(p);
  printf("la pila è: %B\n", v);
  printf("Metto 0 1 2 e 3\n");
  printStack(p);
  printf("\n");
  elem *nd = pop(p);
  printElem(nd);
  elem *nd2 = top(p);
  printElem(nd2);
  printStack(p);
  return 0;
}
