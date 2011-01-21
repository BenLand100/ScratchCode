#!/usr/bin/python

from numpy import *

def findsolns(p):
    solns = []
    for a in range(1,p):
        for b in range(a,p):
            c = p-a-b
            if c**2 == a**2 + b**2:
                solns.append((a,b,c))
    return solns

count = []
for p in range(1,1001):
    solns = findsolns(p)
    print p,solns
    count.append((p,len(solns)))

print sorted(count,key=lambda x:x[1])[-1]

    
                
