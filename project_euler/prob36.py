#!/usr/bin/python

def int10tostr2(x):
    res = ''
    while x > 0:
        res = str(x%2) + res
        x = x >> 1
    return res

base10pals = map(int,filter(lambda x: x == x[::-1], map(str,[i for i in range(0,1000000)])))
alsobase2 = map(lambda x:int(x[0]),filter(lambda x: x[1] == x[1][::-1], map(lambda x:(x,int10tostr2(x)),base10pals)))

print sum(alsobase2)
