#!/usr/bin/python

from numpy import *

def corneriter():
    side = 3
    corner = 0
    i = 3
    while 1: 
        yield i
        if corner == 3:
            corner = 0
            side+=2
            i+=side-1
        else:
            corner = (corner + 1) % 4
            i+=side-1
 

def isprime(x):
    if x % 2 == 0:
        return 0
    for i in range(3,1+int(floor(sqrt(x))),2):
        if x % i == 0:
            return 0
    return 1

corners = corneriter();
cornernums = [1]
cornerprimes = []
side = 1
for i in [corners.next() for j in range(4)]:
    cornernums.append(i)
    if isprime(i): 
        cornerprimes.append(i)
side += 2
prob = float(len(cornerprimes)) / float(len(cornernums))
print side,prob,cornernums[-1:-5:-1]
while prob >= 0.10:
    for i in [corners.next() for j in range(4)]:
        cornernums.append(i)
        if isprime(i): 
            cornerprimes.append(i)
    side += 2
    prob = float(len(cornerprimes)) / float(len(cornernums))
    print side,prob,cornernums[-1:-5:-1]
                
    
