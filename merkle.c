#include <stdio.h>
#include <stdlib.h>
#include "sha256.h"

#define SEGSIZE 64
#define READSIZE 32

typedef struct stack stack;
typedef struct elem elem;
void push(stack*, elem*);
elem* pop(stack*);
void collapse(stack*);
uint8* root(stack*);
void readFrom(stack*, FILE*);
void printHash(uint8*);

struct stack {
	elem* head;
};

struct elem {
	int   height;
	uint8 sum[32];
	elem* next;
};

sha256_context ctx;

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
	free(oldhead);
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
	int i;
	uint8* leaf = calloc(1, READSIZE);
	while (!ferror(f) && !feof(f)) {
		elem* e = calloc(1, sizeof(elem));
		sha256_starts(&ctx);
		for (i = 0; i < SEGSIZE; i += READSIZE) {
			size_t n = fread(leaf, 1, READSIZE, f);
			sha256_update(&ctx, leaf, n);
		}
		sha256_finish(&ctx, e->sum);
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
