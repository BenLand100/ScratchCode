#!/usr/bin/python


def sum_digits(x):
    val = 0
    while x > 0:
        val += x % 10
        x /= 10
    return val

print sorted([sum_digits(a**b) for a in range(100) for b in range(100)])[-1]
