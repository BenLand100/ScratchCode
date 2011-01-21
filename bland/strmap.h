#ifndef _STRMAP_H
#define	_STRMAP_H

typedef struct {
    int size;
    int used;
    int sorted;
    char** strs;
    int* nums;
} strmap;

#define STRMAP_INITSIZE 500

void strmap_init(strmap *dat);
int strmap_put(strmap *dat, char *wat, int num);
void strmap_sort(strmap *dat);
int strmap_get(strmap *dat, char *wat, int *num);
void strmap_free(strmap *dat);

#endif	/* _STRMAP_H */

