#!/usr/bin/python

from numpy import *

def divisors(x):
    divisors = [];
    for i in range(1,1+int(floor(sqrt(x)))):
        if x % i == 0:
            divisors.append(i)
    divisors.extend(map(lambda i: x/i,divisors))
    return list(set(divisors))

def d(n):
    return sum(divisors(n))-n

vals = [[n,d(n)] for n in range(1,10001)]
twicesum = 0
for i,j in vals:
    if j > 0 and j < 10000:
        if i == vals[j-1][1] and i != j:
            print i,'<>',j
            twicesum += i + j

print twicesum / 2
