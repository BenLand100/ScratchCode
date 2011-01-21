#ifndef _HOOKMAP_H
#define	_HOOKMAP_H

#include "strmap.h"

#define HOOKMAP_INITSIZE 500

typedef void (*hookproc)();

typedef struct {
    strmap cmd_tok;
    int size, used;
    hookproc* map;
} hooks;

void hookmap_init(hooks *dat);
void hookmap_set(hooks *dat, char *wat, hookproc hook);
void hoohmap_run(hooks *dat, char *wat);
void hookmap_free(hooks *dat);

#endif

