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

