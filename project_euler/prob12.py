#!/usr/bin/python

def numfactors(x):
    num = 2
    exps = []
    while x > 1:
        exp = 0
        while x % num == 0:
            exp += 1
            x /= num
        if exp > 0:
            exps.append(exp)
        num += 1
    num = 1
    for exp in exps:
        num *= exp+1
    #print 'factors:',num
    return num

cur = 1
tri = 1
while numfactors(tri) < 500:
    cur += 1
    tri += cur
    #print cur,tri

print tri
