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

#include "numstr.h"
#include <stdlib.h>
#include <string.h>

void numstr_init(numstr *dat) {
    dat->size = NUMSTR_INITSIZE;
    dat->used = 0;
    dat->sorted = 0;
    dat->strs = malloc(NUMSTR_INITSIZE*sizeof(char*));
    dat->nums = malloc(NUMSTR_INITSIZE*sizeof(int*));
}

int numstr_add(numstr *dat, char *wat) {
    dat->sorted++;
    if (dat->used == dat->size) {
        dat->size += NUMSTR_INITSIZE;
        dat->strs = realloc(dat->strs, dat->size*sizeof(char*));
        dat->nums = realloc(dat->nums, dat->size*sizeof(int));
    }
    dat->used;
    dat->strs[dat->used] = malloc(strlen(wat)+1);
    strcpy(dat->strs[dat->used],wat);
    dat->nums[dat->used] = dat->used;
    dat->used++;
}

inline void numstr_swap(char** strs, int* nums, int a, int b) {
    char* s_temp = strs[a];
    strs[a] = strs[b];
    strs[b] = s_temp;
    int i_temp = nums[a];
    nums[a] = nums[b];
    nums[b] = i_temp;
}

int numstr_part(char** strs, int* nums, int left, int mid, int right) {
    char* pivot = strs[mid];
    numstr_swap(strs, nums, mid, right);
    int sto = left, i;
    for (i = left; i < right; i++) {
        if (strcmp(strs[i], pivot) <= 0) {
            numstr_swap(strs, nums, i, sto);
            sto++;
        }
    }
    numstr_swap(strs, nums, sto, right);
    return sto;
}

void numstr_rec_sort(char** strs, int* nums, int left, int right) {
    if (right > left) {
        int mid = (left + right) / 2;
        mid = numstr_part(strs, nums, left, mid, right);
        numstr_rec_sort(strs, nums, left, mid - 1);
        numstr_rec_sort(strs, nums, mid + 1, right);
    }
}

void numstr_sort(numstr *dat) {
    if (!dat->sorted) return;
    numstr_rec_sort(dat->strs,dat->nums,0,dat->used-1);
    dat->sorted = 0;
}

int numstr_tok(numstr *dat, char *wat) {
    if (dat->sorted) numstr_sort(dat);
    char** strs = dat->strs;
    int left = 0, right = dat->used-1, mid, cmp;
    do {
        if (left > right) return -1;
        mid = (left+right)/2;
        cmp = strcmp(wat,strs[mid]);
        if (cmp < 0) {
            right = mid - 1;
        } else {
            left = mid + 1;
        }
    } while (cmp);
    return dat->nums[mid];
}

void numstr_free(numstr *dat) {
    int i;
    for (i = 0; i < dat->used; i++)
        free(dat->strs[i]);
    free(dat->strs);
    free(dat->nums);
}

