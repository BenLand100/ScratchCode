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

maxval = 1000000

primes = primesbelow(maxval)
pset = set(primes)

primeidx = dict([(p,i) for i,p in enumerate(primes)])

maxsum = [0 for x in range(len(primes))]

for i in range(len(primes)):
    val = primes[i]
    for n in range(i+1,len(primes)):
        val += primes[n]
        if val > maxval: break
        if val in pset:
            maxsum[primeidx[val]] = max(maxsum[primeidx[val]],n-i+1)

summap = sorted(map(lambda a,b: (a,b), primes, maxsum),key=lambda x:x[1])
print summap[-1]
            
