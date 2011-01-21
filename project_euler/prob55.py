#!/usr/bin/python

def islychrel(x):
    x += int(str(x)[::-1])
    for i in range(50):
        xstr = str(x)
        revstr = xstr[::-1]
        if xstr == revstr:
            return 0
        x += int(revstr)
    return 1

res = 0
for i in range(0,10000):
    if islychrel(i):
        res+=1

print res


