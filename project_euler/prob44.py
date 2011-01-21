#!/usr/bin/python

from numpy import *

def ispent(x):
    return (sqrt(24*x+1)+1) % 6 == 0

pentags = set([])

for i in range(1,10000):
    a = i*(3*i-1)/2
    pentags.add(a)
    for b in pentags:
        if a-b in pentags and ispent(a+b):
            print a-b
            exit()
