#!/usr/bin/python

def opt_exp(b,e,modmask):
    res = 1
    if e / modmask > 0:
        for i in range(0,modmask):
            res = (res * b) % modmask
    b %= modmask
    for i in range(0,e%modmask):
        if i != 0 and i % modmask == 0: print b,e,res
        res = (res * b) % modmask
    return res

def opt_seq(x,modmask):
    res = 0
    for i in range(1,x+1):
        res = (res + opt_exp(i,i,modmask)) % modmask
    return res % modmask

print opt_seq(1000,10**10)
