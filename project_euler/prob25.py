#!/usr/bin/python

last = 1
fibb = 1
idx = 2

while len(str(fibb)) < 1000:
    next = last + fibb
    last = fibb
    fibb = next
    idx += 1

print idx
