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

abundants = map(lambda x: x[0], filter(lambda x: x[0] < x[1], [[n,d(n)] for n in range(1,28124)]))
sumsoftwo = [a+b for a in abundants for b in abundants]
print sum(list(set(range(1,25000)).difference(set(sumsoftwo))))
