#!/usr/bin/python

from numpy import *

def isprime(x):
    if x < 0 or x % 2 == 0:
        return 0
    for i in range(3,1+int(floor(sqrt(x))),2):
        if x % i == 0:
            return 0
    return 1

def minunprime(poly):
    n = 0    
    while isprime(poly(n)):
        n+=1
    return n-1

data = []
for a in range(-1000,1001):
    for b in range(-1000,1001):
        if isprime(b):
            poly = lambda x: x*x+a*x+b
            if poly(0) > 0:
                val = minunprime(poly)
                if val > 0:
                    data.append((val,a,b))

print map(lambda x: x[1]*x[2],sorted(data,key=lambda x: x[0]))[-1]
        
