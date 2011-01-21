#!/usr/bin/python

def tryfor9mulseq(x):
    s = ''
    n = 1
    while len(s) < 9:
        s+=str(x*n)
        n+=1
    if n <= 2 or len(s) != 9:
        return 0
    return s

def ispandigital9(s):
    for i in ['1','2','3','4','5','6','7','8','9']:
        if s.find(i) == -1:
            return 0
    return 1

pandigitals = []

for i in range(10000):
    seq = tryfor9mulseq(i)
    if seq and ispandigital9(seq):
        print i,seq
        pandigitals.append(int(seq))

print sorted(pandigitals)[-1]

