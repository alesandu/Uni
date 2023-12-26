#include <stdio.h>
#include <stdlib.h>

// Node
typedef struct node {
  int value;
  struct node *sin;
  struct node *des;
} node;

node *initNode();
void printNode(node *);

node *initNode() {
  node *n = malloc(sizeof(node));
  n->value = 0;
  n->sin = NULL;
  n->des = NULL;
  return n;
}

void printNode(node *r) { printf("%d", r->value); }

// Elem
typedef struct elem {
  node nd;
  struct elem *prev;
} elem;

elem *initElem();
void printElem(elem *);

elem *initElem() {
  elem *n = malloc(sizeof(elem));
  node *t = initNode();
  n->nd = *t;
  n->prev = NULL;
  return n;
}

void printElem(elem *e) { printNode(&e->nd); }

// Stack
typedef struct stack {
  elem *en;
} stack;

void printStack(stack *);
stack *initStack();
void push(stack *, elem *);
int isStackEmpty(stack *);
elem *pop(stack *);
elem *top(stack *);

void printStack(stack *p) {
  elem *t;
  t = p->en;
  while (t != NULL) {
    printElem(t);
    t = t->prev;
  }
}

stack *initStack() {
  stack *p = malloc(sizeof(stack));
  p->en = NULL;
  return p;
}

int isStackEmpty(stack *p) {
  int b = 1;
  if (p->en != NULL)
    b = 0;
  return b;
}

void push(stack *p, elem *e) {
  e->prev = p->en;
  p->en = e;
}

elem *pop(stack *p) {
  elem *t;
  t = p->en;
  p->en = t->prev;
  return t;
}

elem *top(stack *p) { return p->en; }

// Queue
typedef struct queue {
  elem *st;
  elem *en;
} queue;

void printQueue(queue *);
queue *initQueue();
void enqueue(queue *, elem *);
int isQueueEmpty(queue *);
elem *dequeue(queue *);
elem *first(queue *);

void printQueue(queue *p) {
  elem *t;
  t = p->st;
  while (t != NULL) {
    printElem(t);
    t = t->prev;
  }
}

queue *initQueue() {
  queue *p = malloc(sizeof(queue));
  p->st = NULL;
  p->en = NULL;
  return p;
}

int isQueueEmpty(queue *p) {
  int b = 1;
  if (p->en != NULL && p->st != NULL)
    b = 0;
  return b;
}

void enqueue(queue *p, elem *e) {
  if (isQueueEmpty(p)) {
    p->st = e;
    p->en = e;
  } else {
    p->en->prev = e;
    p->en = e;
  }
}

elem *dequeue(queue *p) {
  elem *t;
  t = p->st;
  p->st = t->prev;
  return t;
}

elem *first(queue *p) { return p->st; }

// Tree
typedef struct tree {
  node *r;
} tree;

tree *initTree();
void visitaDFS(node *);
void visitaDFSricorsiva(node *);
void visitaBFS(node *);

tree *initTree() {
  tree *t = malloc(sizeof(tree));
  t->r = NULL;
  return t;
}

void visitaDFS(node *r) {
  stack *p = initStack();
  elem *t = initElem();
  elem *ts = initElem();
  elem *td = initElem();
  t->nd = *r;
  push(p, t);
  while (!isStackEmpty(p)) {
    elem *u = pop(p);
    if (u != NULL) {
      printElem(u);
      if ((u->nd.des) != NULL) {
        td->nd = *u->nd.des;
        push(p, td);
      }
      if ((u->nd.sin) != NULL) {
        ts->nd = *u->nd.sin;
        push(p, ts);
      }
    }
  }
  free(t);
  free(ts);
  free(td);
  free(p);
}

void visitaDFSricorsiva(node *r) {
  if (r != NULL) {
    printNode(r);
    visitaDFSricorsiva(r->sin);
    visitaDFSricorsiva(r->des);
  }
}

void visitaBFS(node *r) {
  queue *p = initQueue();
  elem *t = initElem();
  elem *ts = initElem();
  elem *td = initElem();
  t->nd = *r;
  enqueue(p, t);
  while (!isQueueEmpty(p)) {
    elem *u = dequeue(p);
    if (u != NULL) {
      printElem(u);
      if ((u->nd.sin) != NULL) {
        ts->nd = *u->nd.sin;
        enqueue(p, ts);
      }
      if ((u->nd.des) != NULL) {
        td->nd = *u->nd.des;
        enqueue(p, td);
      }
    }
  }
  free(t);
  free(ts);
  free(td);
  free(p);
}

int main() {
  tree *t = initTree();
  node *a = initNode();
  node *l = initNode();
  node *b = initNode();
  node *e = initNode();
  node *r = initNode();
  node *o = initNode();
  a->value = 1;
  l->value = 2;
  b->value = 3;
  e->value = 4;
  r->value = 5;
  o->value = 6;
  a->sin = l;
  a->des = b;
  l->sin = e;
  l->des = r;
  b->sin = o;
  t->r = a;
  visitaDFS(t->r);
  printf("\n");

  //visitaBFS(t->r);
  printf("\n");

  //visitaDFSricorsiva(t->r);

  return 0;
}
