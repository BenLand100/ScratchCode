#!/usr/bin/python

def sumofsquares(x):
    nums = range(1,x+1)
    squares = map(lambda x: x*x, nums)
    return sum(squares)

def squareofsum(x):
    nums = range(1,x+1)
    return sum(nums)**2

print squareofsum(100) - sumofsquares(100)
