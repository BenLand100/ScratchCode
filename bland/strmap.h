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

