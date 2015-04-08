#include <stdio.h>
#include <stdlib.h>
#include "sha256.h"

#define LEAFSIZE 64

// forward declarations
typedef struct stack stack;
typedef struct elem elem;
void push(stack*, elem*);
elem* pop(stack*);
void collapse(stack*);
uint8* root(stack*);
void readFrom(stack*, FILE*);
void printHash(uint8*);

// types
struct stack {
	elem* head;
};

struct elem {
	int   height;
	uint8 sum[32];
	elem* next;
};

// global variables
sha256_context ctx;

// function definitions
void push(stack* s, elem* e) {
	e->next = s->head;
	s->head = e;
	while (s->head->next != NULL && s->head->height == s->head->next->height) {
		collapse(s);
	}
}

elem* pop(stack* s) {
	elem* e = s->head;
	s->head = s->head->next;
	return e;
}

void collapse(stack* s) {
	elem* oldhead = pop(s);
	sha256_starts(&ctx);
	sha256_update(&ctx, s->head->sum, 32);
	sha256_update(&ctx, oldhead->sum, 32);
	sha256_finish(&ctx, s->head->sum);
	s->head->height++;
}

uint8* root(stack* s) {
	if (s->head == NULL) {
		return NULL;
	}
	while (s->head->next != NULL) {
		collapse(s);
	}
	return s->head->sum;
}

void readFrom(stack* s, FILE* f) {
	while (!ferror(f) && !feof(f)) {
		// create elem
		elem* e = calloc(1, sizeof(elem));
		// read data
		uint8* leaf = calloc(1, LEAFSIZE);
		size_t n = fread(leaf, 1, LEAFSIZE, f);
		// calculate hash
		sha256_starts(&ctx);
		sha256_update(&ctx, leaf, n);
		sha256_finish(&ctx, e->sum);
		// push
		push(s, e);
	}
}

void printHash(uint8* hash) {
	int i;
	for (i = 0; i < 32; i++) {
		printf("%.2x", hash[i]);
	}
	printf("\n");
}

int main() {
	FILE* f = fopen("test.dat", "r");
	if (f == NULL) {
		printf("couldn't open file");
		return 1;
	}

	stack s;
	s.head = NULL;
	readFrom(&s, f);

	uint8* merkleRoot = root(&s);
	if (merkleRoot == NULL) {
		printf("couldn't calculate Merkle root");
		return 1;
	}
	printHash(merkleRoot);

	fclose(f);
	return 0;
}