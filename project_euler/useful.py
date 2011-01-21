#!/usr/bin/python

from numpy import *

def perm(l):
    sz = len(l)
    if sz <= 1:
        return [l]
    return [p[:i]+[l[0]]+p[i:] for i in xrange(sz) for p in perm(l[1:])]

def mul(x):
    res = 1
    for i in x:
        res*=i
    return res

def primesbelow(x):
    nums = range(x)
    mask = [i%2 for i in nums]
    mask[1] = 0
    mask[2] = 1
    i = 3
    while i < x:
        if mask[i]:
            for j in range(i*i,x,i):
                mask[j] = 0
        i+=2
    return filter(lambda x: mask[x],nums)

def isprime(x):
    if x % 2 == 0:
        return 0
    for i in range(3,1+int(floor(sqrt(x))),2):
        if x % i == 0:
            return 0
    return 1

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

def gcf(a,b):
    af = primefactors(a)
    bf = primefactors(b)
    cf = []
    i,j = 0,0
    while i < len(af) and j < len(bf):
        if af[i] == bf[j]:
            cf.append(af[i])
            i += 1
            j += 1
        elif af[i] < bf[i]:
            i += 1
        else:   
            j += 1
    res = 1
    for i in cf:
        res *= i
    return res

def divisors(x):
    divisors = [];
    for i in range(1,1+int(floor(sqrt(x)))):
        if x % i == 0:
            divisors.append(i)
    divisors.extend(map(lambda i: x/i,divisors))
    return list(set(divisors))


def combine(seq):
    res = 0
    for i in range(len(seq)):
        res += seq[-i-1] * 10**i
    return res

def sum_digits(x):
    val = 0
    while x > 0:
        val += x % 10
        x /= 10
    return val

def factorial(x):
    res = 1
    for i in range(1,x+1):
        res *= i
    return res

def mod_exp(b,e,modmask):
    res = 1
    if e / modmask > 0:
        for i in range(0,modmask):
            res = (res * b) % modmask
    b %= modmask
    for i in range(0,e%modmask):
        if i != 0 and i % modmask == 0: print b,e,res
        res = (res * b) % modmask
    return res
