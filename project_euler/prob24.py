#!/usr/bin/python

from types import *

def permute10(vals):
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
                                        for j in range(0,len(vals)):
                                            if j in [a,b,c,d,e,f,g,h,i]: continue
                                            yield [vals[a],vals[b],vals[c],vals[d],vals[e],vals[f],vals[g],vals[h],vals[i],vals[j]]
                    

perm = permute10([0,1,2,3,4,5,6,7,8,9])
for i in range(1,1000000):
    perm.next()
print perm.next()
    

