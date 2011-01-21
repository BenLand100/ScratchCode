#!/usr/bin/python

def permute10(vals):
    for a in range(0,len(vals)):
        for b in range(0,len(vals)):
            if b in [a]: continue
            for c in range(0,len(vals)):
                if c in [a,b]: continue
                for d in range(0,len(vals)):
                    if d in [a,b,c]: continue
                    for e in range(0,len(vals)):
                        if e in [a,b,c,d]: continue
                        for f in range(0,len(vals)):
                            if f in [a,b,c,d,e]: continue
                            for g in range(0,len(vals)):
                                if g in [a,b,c,d,e,f]: continue
                                for h in range(0,len(vals)):
                                    if h in [a,b,c,d,e,f,g]: continue
                                    for i in range(0,len(vals)):
                                        if i in [a,b,c,d,e,f,g,h]: continue
                                        for j in range(0,len(vals)):
                                            if j in [a,b,c,d,e,f,g,h,i]: continue
                                            yield [vals[a],vals[b],vals[c],vals[d],vals[e],vals[f],vals[g],vals[h],vals[i],vals[j]]


def combine(seq):
    res = 0
    for i in range(len(seq)):
        res += seq[-i-1] * 10**i
    return res

def subdivprop(seq):
    if combine(seq[1:4]) % 2 == 0:
        if combine(seq[2:5]) % 3 == 0:
            if combine(seq[3:6]) % 5 == 0:
                if combine(seq[4:7]) % 7 == 0:
                    if combine(seq[5:8]) % 11 == 0:
                        if combine(seq[6:9]) % 13 == 0:
                            if combine(seq[7:10]) % 17 == 0:
                                return 1
    return 0


perm = permute10([0,1,2,3,4,5,6,7,8,9])
val = 0
for i in range(10*9*8*7*6*5*4*3*2*1):
    pan = perm.next()
    if subdivprop(pan):
        print pan
        val += combine(pan)

print val
