/**
 *  Copyright 2011 by Benjamin J. Land (a.k.a. BenLand100)
 *
 *  This file is part of ScratchCode.
 *
 *  ScratchCode is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  ScratchCode is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ScratchCode. If not, see <http://www.gnu.org/licenses/>.
 */

#include "hookmap.h"
#include <stdlib.h>

void hookmap_init(hooks *dat) {
    numstr_init(&dat->cmd_tok);
    dat->map = malloc(HOOKMAP_INITSIZE*sizeof(hookproc));
    dat->size = HOOKMAP_INITSIZE;
    dat->used = 0;
}

void hookmap_free(hooks *dat) {
    numstr_free(&dat->cmd_tok);
    free(dat->map);
}

void hookmap_set(hooks *dat, char *cmd, hookproc hook) {
    if (numstr_tok(&dat->cmd_tok,cmd) == -1) {
        numstr_add(&dat->cmd_tok,cmd);
        if (dat->used == dat->size) {
            dat->size += 500;
            dat->map = realloc(dat->map,dat->size);
        }
        dat->used++;
    }
    int idx = numstr_tok(&dat->cmd_tok,cmd);
    dat->map[idx] = hook;
}

void hookmap_run(hooks *dat, char *cmd) {
    int idx = numstr_tok(&dat->cmd_tok,cmd);
    if (idx != -1 && dat->map[idx]) {
        dat->map[idx]();
    }
}
