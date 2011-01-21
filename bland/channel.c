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
