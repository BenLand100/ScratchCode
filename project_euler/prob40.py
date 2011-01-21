#!/usr/bin/python

def findnth(vals):
    vals = sorted(vals)
    res = []
    idx = 0 
    num = 0
    for v in vals:
        while idx < v:
            num+=1
            idx += len(str(num))
        back = idx - v + 1
        res.append((v,int(str(num)[-back])))
    return res

def mul(x):
    res = 1
    for i in x:
        res*=i
    return res

nth = findnth([1,10,100,1000,10000,100000,1000000])
print nth
print mul([x[1] for x in nth])

