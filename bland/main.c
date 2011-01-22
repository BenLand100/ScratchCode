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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdarg.h>
#include <regex.h>

#include "hookmap.h"
#include "strmap.h"
#include "channel.h"

#define SERV "irc.rizon.net"
#define PORT 6667
#define CHAN "#SRL"
#define NICK "BLand"
#define USER "gcc"
#define REAL "GNU Compiler Collection"
#define PASS "---"

#define MASTER "BENLAND100"

int irc_conn;
char send_buffer[513];
char read_buffer[513];

hooks irc_hooks;
hooks user_hooks;
strmap access;
strmap levels;

regex_t sed_match;

channel_data *channels;
strmap chanmap;
int chancount;


struct {
    union {
        char* servername;
        struct {
            char *nick, *user, *host;
        };
    };
    char* cmd;
    int nargs;
    char** args;
    char* target;
} dat;

#define debug(...) _debug(__VA_ARGS__);
#define debug(...)
void _debug(const char *ctrl, ...) {
    va_list ap;
    va_start(ap, ctrl);
    vprintf(ctrl, ap);
    va_end(ap);
}

void fatal(const char *ctrl, ...) {
    va_list ap;
    va_start(ap, ctrl);
    vprintf(ctrl, ap);
    va_end(ap);
    exit(-1);
}

inline void upper(char* str) {
    while (*str) {
        if (*str >= 'a' && *str <= 'z')
            *str = *str - 'a' + 'A';
        str++;
    }
}

inline char charupper(char c) {
    return c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c;
}

inline int strcmpi(char* str1, char* str2) {
    for (;;) {
        char c1 = charupper(*(str1++));
        char c2 = charupper(*(str2++));
        if (!c1 || c1 != c2) return c1 - c2;
    }
}

channel_data* getChannel(char* name) {
    static char buffer[150];
    snprintf(buffer,150,name);
    upper(buffer);
    int idx;
    if (strmap_get(&chanmap,buffer,&idx) != -1) {
        return &channels[idx];
    } else {
        idx = chancount++;
        channels = realloc(channels,chancount*sizeof(channel_data));
        strmap_put(&chanmap,buffer,idx);
        channel_init(&channels[idx]);
        return &channels[idx];
    }
}

void sethook_irc(char* wat, hookproc hook) {
    hookmap_set(&irc_hooks,wat,hook);
}

void runhook_irc(char* wat) {
    hookmap_run(&irc_hooks,wat);
}

void sethook_user(char* wat, int level, hookproc hook) {
    strmap_put(&levels,wat,level);
    hookmap_set(&user_hooks,wat,hook);
}

void runhook_user(char* wat, char* who) {
    char* new = malloc(strlen(who)+1);
    strcpy(new,who);
    upper(new);
    int min, val;
    if (strmap_get(&access,new,&val) < 0) val = 0;
    if (strmap_get(&levels,wat,&min) < 0) return;
    if (val >= min) hookmap_run(&user_hooks,wat);
}

int irc_socket(const char *hostname, int port) {
    struct sockaddr_in name;
    struct hostent *hostinfo;
    debug("Attempting to connect to %s:%i\n", hostname, port);
    name.sin_family = AF_INET;
    name.sin_port = htons(port);
    if ((hostinfo = gethostbyname(hostname)) == NULL)
        fatal("Unknown host %s.\n", hostname);
    name.sin_addr = *(struct in_addr *) hostinfo->h_addr;
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(sock, (struct sockaddr*) &name, sizeof(name)) == -1)
        fatal("Could not bind socket\n");
    return sock;
}

inline int haschar(char* str, char c) {
    while (*str && (*str != c)) str++;
    return *str == c;
}

void parseArgs(char* buff, int count) {
    while (*buff == ' ') buff++;
    switch (*buff) {
        case '\r':
        case '\n':
            dat.nargs = count;
            dat.args = realloc(dat.args,count*sizeof(char*));
            return;
        case ':':
            dat.nargs = count+1;
            dat.args = realloc(dat.args,(count+1)*sizeof(char*));
            dat.args[count] = ++buff;
            while (*buff != '\r' && *buff != '\n') buff++;
            *buff = 0;
            return;
    }
    char* start = buff;
    for (;;) {
        switch (*buff) {
            case '\r':
            case '\n':
                *buff = 0;
                dat.nargs = count+1;
                dat.args = realloc(dat.args,(count+1)*sizeof(char*));
                dat.args[count] = start;
                return;
            case ' ':
                *buff = 0;
                parseArgs(++buff,count+1);
                dat.args[count] = start;
                return;
            default:
                buff++;
        }
    }
}

void irc_read() {
    int len = 0;
    do {
        len += read(irc_conn, read_buffer + len, 1);
    } while (read_buffer[len-1] != '\n');
    read_buffer[len] = 0;
    debug(">>%s",read_buffer);
    char* buff = read_buffer;
    source_parse:
    if (*buff == ':') {
        dat.nick = ++buff;
        dat.user = 0;
        dat.host = 0;
        for (;;) {
            switch (*buff) {
                case '!':
                    *(buff++) = 0;
                    dat.user = buff;
                    break;
                case '@':
                    *(buff++) = 0;
                    dat.host = buff;
                    break;
                case ' ':
                    *(buff++) = 0;
                    goto cmd_parse;
                default:
                    buff++;
            }
        }
    } else {
        dat.nick = 0;
        dat.user = 0;
        dat.host = 0;
    }
    cmd_parse:
    debug("Nick: %s User: %s Host: %s\n",dat.nick, dat.user,dat.host);
    while (*buff == ' ') buff++;
    dat.cmd = buff++;
    while (*buff != ' ') buff++;
    *(buff++) = 0;
    upper(dat.cmd);
    parseArgs(buff,0);
}

void irc_send() {
    int end;
    for (end = 0; end < 513; end++) if (send_buffer[end] == 0) break;
    printf("<<%s",send_buffer);
    int left = end;
    while (left) {
        int written = write(irc_conn, send_buffer + end - left, left);
        if (written == -1) fatal("Socket failed on send\n");
        left -= written;
    }
}

void firc_send(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int len = vsnprintf(send_buffer,513,fmt,ap);
    va_end(ap);
    len = len > 510 ? 510 : len;
    send_buffer[len++] = '\r';
    send_buffer[len++] = '\n';
    send_buffer[len++] = 0;
    irc_send();
}

void login(const char *nick, const char *user, const char *real) {
    firc_send("USER %s 6 6 :%s", user, real);
    firc_send("NICK %s", nick);
}


void init() {
    irc_conn = irc_socket(SERV,PORT);
    dat.args = malloc(0);
    hookmap_init(&irc_hooks);
    hookmap_init(&user_hooks);
    strmap_init(&access);
    strmap_init(&levels);
    strmap_put(&access,MASTER,100);
    memset(&sed_match,0,sizeof(sed_match));
    regcomp(&sed_match,"^s(.)((\\\\.|.)*)\\1((\\\\.|.)*)\\1([^; ]*)",REG_EXTENDED);
    strmap_init(&chanmap);
    channels = malloc(0);
    chancount = 0;
}

void term() {
    printf("Shutting down\n");

    shutdown(irc_conn,2);
    free(dat.args);
    hookmap_free(&irc_hooks);
    hookmap_free(&user_hooks);
    regfree(&sed_match);
    strmap_free(&chanmap);
    int i;
    for (i = 0; i < chancount; i++) {
        channel_free(&channels[i]);
    }
    free(channels);
    exit(EXIT_SUCCESS);
}

void end_motd() {
    #ifdef PASS
    firc_send("PRIVMSG NickServ :IDENTIFY %s", PASS);
    #endif
    firc_send("JOIN %s",CHAN);
}

void privmsg() {
    char* source = dat.nick;
    char* dest = dat.args[0];
    char* message = dat.args[1];
    dat.target = strcasecmp(dest,NICK) ? dest : source;
    channel_data *channel = getChannel(dat.target);
    printf("<%s:%s> %s\n",source,dest,message);
    switch (*message) {
        case '!': {
            if (message[1] == 0 || message[1] == '\n' || message[1] == '\r') return;
            char* cmd = strtok(message+1," \r\n");
            upper(cmd);
            if (cmd) runhook_user(cmd,source);
        } return;
        case 's': {
            regmatch_t *result = malloc(sizeof(regmatch_t) * (sed_match.re_nsub+1));
            if (regexec(&sed_match,message,sed_match.re_nsub+1,result,0)) goto regex_failed;

            char* reg = malloc(result[2].rm_eo-result[2].rm_so+1);
            memcpy(reg,message+result[2].rm_so,result[2].rm_eo-result[2].rm_so);
            reg[result[2].rm_eo-result[2].rm_so] = 0;
            char* rep = malloc(result[4].rm_eo-result[4].rm_so+1);
            memcpy(rep,message+result[4].rm_so,result[4].rm_eo-result[4].rm_so);
            rep[result[4].rm_eo-result[4].rm_so] = 0;
            char* ctrl = malloc(result[6].rm_eo-result[6].rm_so+1);
            memcpy(ctrl,message+result[6].rm_so,result[6].rm_eo-result[6].rm_so);
            ctrl[result[4].rm_eo-result[6].rm_so] = 0;

            regex_t irc_reg;
            if (regcomp(&irc_reg,reg,REG_EXTENDED)) goto regex_failed;
            regmatch_t *irc_res = malloc(sizeof(regmatch_t) * (irc_reg.re_nsub+1));
            int replen = strlen(rep);
            int i;
            for (i = 0; i < HISTORY; i++) {
                history_t *source = &channel->history[i];
                char* search =  source->message;
                char *result = 0;
                int offset = 0;
                int k = 0, len = 0;
                do {
                    if (regexec(&irc_reg,search+offset,irc_reg.re_nsub+1,irc_res,0) == 0) {
                        int matchsize = irc_res[0].rm_eo - irc_res[0].rm_so;
                        if (result == 0) {
                            len = irc_res[0].rm_so + 1;
                            result = malloc(len);
                            memcpy(result,search,irc_res[0].rm_so);
                            k = irc_res[0].rm_so;
                        } else {
                            int midlen = irc_res[0].rm_so;
                            len += midlen;
                            result = realloc(result,len);
                            memcpy(result+k,search+offset,midlen);
                            k += midlen;
                        }
                        len += replen;
                        result = realloc(result,len);
                        int j;
                        for (j = 0; j < replen; ) {
                            switch (rep[j]) {
                                case '\\':
                                    switch (rep[++j]) {
                                        case '1':
                                        case '2':
                                        case '3':
                                        case '4':
                                        case '5':
                                        case '6':
                                        case '7':
                                        case '8':
                                        case '9': {
                                            int idx = rep[j++] - '1' + 1;
                                            if (idx <= irc_reg.re_nsub) {
                                                len += irc_res[idx].rm_eo - irc_res[idx].rm_so;
                                                result = realloc(result,len);
                                                memcpy(result+k,search+offset+irc_res[idx].rm_so,irc_res[idx].rm_eo - irc_res[idx].rm_so);
                                                k += matchsize;
                                            } else {
                                                len -= 2;
                                            }
                                        } break;
                                        default:
                                            len--;
                                            result[k++] = rep[j++];
                                    } break;
                                case '&':
                                    len += matchsize;
                                    result = realloc(result,len);
                                    memcpy(result+k,search+offset+irc_res[0].rm_so,matchsize);
                                    k += matchsize;
                                    j++;
                                    break;
                                default:
                                    result[k++] = rep[j++];
                            }
                        }
                        offset += irc_res[0].rm_eo;
                    } else {
                        break;
                    }
                } while(haschar(ctrl,'g'));
                if (result) {
                    int endlen = strlen(search)-offset;
                    len += endlen;
                    result = realloc(result,len);
                    memcpy(result+k,search+offset,endlen);
                    k += endlen;
                    result[k] = 0;
                    firc_send("PRIVMSG %s :<%s> %s",dat.target,source->nick,result);
                    channel_push_history(channel,source->nick,result);
                    free(result);
                    break;
                }
            }
            free(irc_res);
            regfree(&irc_reg);
            regex_failed:
            free(result);
        } return;
        default:
            channel_push_history(channel,dat.nick,message);
    }
}

void notice() {
    char* source = dat.nick;
    char* dest = dat.args[0];
    char* message = dat.args[1];
    dat.target = strcmpi(dest,NICK) ? dest : source;
    printf("<%s|%s> %s\n",source,dest,message);
}

void error() {
    printf("ERROR: %s\n", dat.args[0]);
    term();
}

void ping() {
    printf("Responding to PING\n");
    firc_send("PONG :%s", dat.args[0]);
}

void say() {
    firc_send("PRIVMSG %s :%s", dat.target, strtok(0,"\r\n"));
}

void quit() {
    char* message = strtok(0,"\r\n");
    firc_send("QUIT :%s", message ? message : "Shutting Down");
    term();
}

void part() {
    char *chan = strtok(0," \r\n");
    char *message = strtok(0,"\r\n");
    if (chan) firc_send("PART %s :%s", chan, message ? message : "Leaving");
}

void join() {
    char *chan = strtok(0," \r\n");
    if (chan) firc_send("JOIN %s", chan);
}

void add() {
    char *who = strtok(0," \r\n");
    char *acc = strtok(0,"\r\n");
    if (who && acc) {
        upper(who);
        upper(dat.nick);
        int max;
        if (strmap_get(&access,dat.nick,&max) < 0) max = 0;
        int val = atoi(acc);
        if (val < max) strmap_put(&access,who,val);
    }
}

void rem() {
    char *who = strtok(0," \r\n");
    if (who) {
        upper(who);
        upper(dat.nick);
        int max, val;
        if (strmap_get(&access,dat.nick,&max) < 0) max = 0;
        if (strmap_get(&access,who,&val) < 0) val = 0;
        if (val < max) strmap_rem(&access,who);
    }
}

int main(int argc, char** argv) {
    init();
    sethook_irc("376",&end_motd);
    sethook_irc("PING",&ping);
    sethook_irc("ERROR",&error);
    sethook_irc("NOTICE",&notice);
    sethook_irc("PRIVMSG",&privmsg);
    login(NICK,USER,REAL);
    sethook_user("SAY",5,&say);
    sethook_user("ADD",0,&add);
    sethook_user("REM",0,&rem);
    sethook_user("PART",50,&part);
    sethook_user("JOIN",50,&join);
    sethook_user("QUIT",75,&quit);


    for (;;) {
        irc_read();
        runhook_irc(dat.cmd);
    }
    
}
