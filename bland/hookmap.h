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

