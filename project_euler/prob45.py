#!/usr/bin/python

tria = lambda x:x*(x+1)/2
pent = lambda x:x*(3*x-1)/2
hexa = lambda x:x*(2*x-1)

ti,pi,hi = 286,166,144
tn,pn,hn = tria(ti),pent(pi),hexa(hi)

while tn != pn or pn != hn:
    while tn < hn and tn < pn:
        ti+=1
        tn=tria(ti)
    while pn < hn and pn < tn:
        pi+=1
        pn=pent(pi)
    while hn < pn and hn < tn:
        hi+=1
        hn=hexa(hi)
    while hn > pn and hn > tn:
        pi+=1
        pn=pent(pi)
        ti+=1
        tn=tria(ti)
    while pn > hn and pn > tn:
        ti+=1
        tn=tria(ti)
        hi+=1
        hn=hexa(hi)
    while tn > pn and tn > hn:
        hi+=1
        hn=hexa(hi)
        pi+=1
        pn=pent(pi)
    print 't:',tn,'p:',pn,'h:',hn
    
