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

#include "channel.h"
#include <stdlib.h>

void channel_init(channel_data *dat) {
    dat->history = malloc(sizeof(history_t)*HISTORY);
    int i;
    for (i = 0; i < HISTORY; i++) {
        dat->history[i].nick = malloc(1);
        *dat->history[i].nick = 0;
        dat->history[i].message = malloc(1);
        *dat->history[i].message = 0;
    }
}

void channel_free(channel_data *dat) {
    int i;
    for (i = 0; i < HISTORY; i++) {
        free(dat->history[i].nick);
        free(dat->history[i].message);
    }
    free(dat->history);
}

void channel_push_history(channel_data *dat, char *nick, char *message) {
    int c;
    history_t *history = dat->history;
    char* messagelast = history[HISTORY-1].message;
    char* nicklast = history[HISTORY-1].nick;
    for (c = HISTORY-1; c > 0; c--) {
        history[c].message = history[c-1].message;
        history[c].nick = history[c-1].nick;
    }
    history[0].message = realloc(messagelast,strlen(message)+1);
    strcpy(history[0].message,message);
    history[0].nick = realloc(nicklast,strlen(nick)+1);
    strcpy(history[0].nick,nick);

}
