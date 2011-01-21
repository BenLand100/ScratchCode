#!/usr/bin/python

from numpy import *

def permute7(vals):
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
                                yield [vals[a],vals[b],vals[c],vals[d],vals[e],vals[f],vals[g]]

def combine(seq):
    res = 0
    for i in range(len(seq)):
        res += seq[-i-1] * 10**i
    return res

def isprime(x):
    if x % 2 == 0:
        return 0
    for i in range(3,1+int(floor(sqrt(x))),2):
        if x % i == 0:
            return 0
    return 1

perm = permute7([1,2,3,4,5,6,7])
print 'calculating pandigitals'
pandigital = [combine(perm.next()) for i in range(7*6*5*4*3*2*1)]
print 'determining primes'
primepans = filter(isprime,pandigital)
print 'sorting'
print sorted(primepans)[-1]
