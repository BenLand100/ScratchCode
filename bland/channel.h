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

#ifndef _CHANNEL_H
#define	_CHANNEL_H

#define HISTORY 15

typedef struct {
    char *nick;
    char *message;
} history_t;

typedef struct {
    history_t *history;
} channel_data;

void channel_init(channel_data *dat);
void channel_push_history(channel_data *dat, char *name, char *message);
void channel_free(channel_data *dat);


#endif	/* _CHANNEL_H */

