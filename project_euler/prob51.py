#!/usr/bin/python

from string import replace

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

primes = primesbelow(1000000)
pset = set(primes)

for p in primes:
    s = str(p)
    oc = [(d, s.count(d)) for d in list(s)]
    #print p,oc
    for d,c in oc:
        for i in range(1,c+1):    
            count = 0       
            for r in ['0','1','2','3','4','5','6','7','8','9']:
                n = replace(s,d,r,i)
                #print '    ',i,d,r,int(n)
                if int(replace(s,d,r,i)) in pset and n[0] != '0':
                    count += 1
            if count == 8:
                print p
                exit()
                
