#!/usr/bin/python

def primefactors(x):
    factors = []
    while x % 2 == 0: 
        factors.append(2)
        x /= 2
    factor = 3
    while x > 1:
        while x % factor == 0:
            factors.append(factor)
            x /= factor
        factor += 2
    return factors

i = 2
while 1:
    a = set(primefactors(i))
    if len(a) == 4:
        b = set(primefactors(i+1))
        if len(b) == 4:
            c = set(primefactors(i+2))
            if len(c) == 4:
                d = set(primefactors(i+3))
                if len(d) == 4:
                    print i
                    exit()
    i += 1
