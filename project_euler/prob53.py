#!/usr/bin/python

def factorial(x):
    res = 1
    for i in range(1,x+1):
        res *= i
    return res

fact = [factorial(i) for i in range(101)]

count = 0
for n in range(1,101):
    for r in range(1,n+1):
        if fact[n]/(fact[r]*fact[n-r]) > 1000000:
            count += 1

print count
