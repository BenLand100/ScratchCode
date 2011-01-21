#!/usr/bin/python

def not_divisible(x,vals):
    for i in vals:
        if x % i == 0:
            return 0
    return 1

def nprimes(n):
    num = 2
    primes = []
    while len(primes) < n:
        if not_divisible(num,primes):
            primes.append(num)
        num += 1
    return primes

print nprimes(10001)[-1]

