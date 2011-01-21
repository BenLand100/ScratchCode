#!/usr/bin/python

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

primes = filter(lambda x: len(str(x)) == 4, primesbelow(10000))
pset = set(primes)

for i in range(len(primes)):
    p1 = primes[i]
    for p2 in primes[i+1:len(primes)]:
        step = p2 - p1
        p3 = p2 + step
        if p3 in pset:
            a = set(str(p1))
            b = set(str(p2))
            c = set(str(p3))
            if len(a-b) == len(b-c) == len(c-a) == 0:
                print str(p1)+str(p2)+str(p3)
