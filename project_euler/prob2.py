#!/usr/bin/python

last = 0
fibb = 1
val = 0

while fibb < 4000000:
    if fibb % 2 == 0:
        val += fibb
    next = last + fibb
    last = fibb
    fibb = next

print val
