#!/usr/bin/python

from string import *

x = 1
c = 1
while c:
    if x % 10000 == 0: print x
    s = str(x)
    m = set([(int(d),count(s,d)) for d in list(s)])
    valid = 1
    for y in [x*n for n in range(2,7)]:
        s = str(y)
        if m != set([(int(d),count(s,d)) for d in list(s)]):
            valid = 0
            break
    if valid:
        print x
        c = 0
    x += 1
