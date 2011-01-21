#!/usr/bin/python

def factorial(x):
    res = 1
    for i in range(1,x+1):
        res *= i
    return res

num = 3
val = 0
while num < 50000:
    x = num
    y = 0
    while x > 0:
        y += factorial(x%10)
        x /= 10
    if num == y:
        val += y
    num += 1

print val
