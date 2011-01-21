#!/usr/bin/python

def sum_digits(x):
    val = 0
    while x > 0:
        val += x % 10
        x /= 10
    return val

def factorial(x):
    res = 1
    for i in range(1,x+1):
        res *= i
    return res

print sum_digits(factorial(100))
