#!/usr/bin/python

def not_divisible(x,vals):
    for i in vals:
        if x % i == 0:
            return 0
    return 1

def primes_below(n):
    num = 2
    primes = [2]
    while primes[-1] < n:
        if not_divisible(num,primes):
            primes.append(num)
        num += 1
        if num % 1000 == 0:
            print num
    return primes[0:len(primes)-1]

print sum(primes_below(2000000))

