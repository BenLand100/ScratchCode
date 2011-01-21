#!/usr/bin/python

def permute9(vals):
    for a in range(0,len(vals)):
        for b in range(0,len(vals)):
            if b in [a]: continue
            for c in range(0,len(vals)):
                if c in [a,b]: continue
                for d in range(0,len(vals)):
                    if d in [a,b,c]: continue
                    for e in range(0,len(vals)):
                        if e in [a,b,c,d]: continue
                        for f in range(0,len(vals)):
                            if f in [a,b,c,d,e]: continue
                            for g in range(0,len(vals)):
                                if g in [a,b,c,d,e,f]: continue
                                for h in range(0,len(vals)):
                                    if h in [a,b,c,d,e,f,g]: continue
                                    for i in range(0,len(vals)):
                                        if i in [a,b,c,d,e,f,g,h]: continue
                                        yield [vals[a],vals[b],vals[c],vals[d],vals[e],vals[f],vals[g],vals[h],vals[i]]

def combine(seq):
    res = 0
    for i in range(len(seq)):
        res += seq[-i-1] * 10**i
    return res

def makesets(num):
    sets = []
    for i in range(1,len(num)-1):
        for j in range(i+1,len(num)):
            dat = (combine(num[0:i]),combine(num[i:j]),combine(num[j:]))
            if dat[0]*dat[1] == dat[2]:
                sets.append(dat)
    return sets

perm = permute9([1,2,3,4,5,6,7,8,9])
print 'calculating pandigitals'
pandigital = [perm.next() for i in range(9*8*7*6*5*4*3*2*1)]
print 'forming product sets'
sets = []
for n in pandigital:
    sets.extend(makesets(n))
print 'listing products'
nums = set([x[2] for x in sets])
print 'adding products'
print sum(list(nums))
    
