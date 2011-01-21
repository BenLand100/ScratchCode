#ifndef _NUMSTR_H
#define	_NUMSTR_H


typedef struct {
    int size;
    int used;
    int sorted;
    char** strs;
    int* nums;
} numstr;

#define NUMSTR_INITSIZE 500

void numstr_init(numstr *dat);
int numstr_add(numstr *dat, char *wat);
void numstr_sort(numstr *dat);
int numstr_tok(numstr *dat, char *wat);
void numstr_free(numstr *dat);

#endif
