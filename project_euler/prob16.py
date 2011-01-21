#!/usr/bin/python

def sum_digits(x):
    val = 0
    while x > 0:
        val += x % 10
        x /= 10
    return val

print sum_digits(2**1000)

    
