#!/usr/bin/python

def seqlen(x):
    print x
    length = 0
    while x != 1:
        if x % 2:
            x = 3*x+1
        else:
            x = x/2
        length += 1
    return length

lengths = [[x,seqlen(x)] for x in range(1,1000000)]
print sorted(lengths,key=lambda v:v[1])[-1][0]
