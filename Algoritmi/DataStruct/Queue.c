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

typedef struct queue {
  elem *en;
  elem *st;
} queue;

void printQueue(queue *);
queue *initQueue();
void enqueue(queue *, elem *);
int isQueueEmpty(queue *);
elem *dequeue(queue *);
elem *first(queue *);

void printQueue(queue *p) {
  elem *t = initElem();
  t = p->en;
  while (t != NULL) {
    printf("%d ", t->value);
    t = t->prev;
  }
  free(t);
}

queue *initQueue() {
  queue *p = malloc(sizeof(queue));
  p->en = NULL;
  p->st = NULL;
  return p;
}

int isQueueEmpty(queue *p) {
  int b = 1;
  if (p->st != NULL && p->en != NULL)
    b = 0;
  return b;
}

void enqueue(queue *p, elem *e) {
  if (isQueueEmpty(p)) {
    p->en = e;
    p->st = e;
  } else {
    p->st->prev = e;
    p->st = e;
  }
}

elem *dequeue(queue *p) {
  elem *t = initElem();
  t = p->en;
  p->en = t->prev;
  return t;
  free(t);
}

elem *first(queue *p) { return p->en; }

int main() {
  queue *p = initQueue();
  int v = isQueueEmpty(p);
  printf("la coda è: %B\n", v);
  for (int i = 0; i < 4; i++) {
    elem *e = initElem();
    e->value = i;
    enqueue(p, e);
  }
  v = isQueueEmpty(p);
  printf("la coda è: %B\n", v);
  printf("Metto 0 1 2 e 3 \n");
  printQueue(p);
  printf("\n");
  elem *nd = dequeue(p);
  printElem(nd);
  elem *nd2 = first(p);
  printElem(nd2);
  printQueue(p);
  return 0;
}
