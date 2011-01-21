#!/usr/bin/python

def repeatingseq(seq):
    for i in range(1,len(seq)/2+1):
        if seq[-1:-i-1:-1] == seq[-i-1:-2*i-1:-1]:
            return seq[-1:-i-1:-1][::-1]
    return 0

def findrepeating(a,b):
    print a,b
    seqs = []
    while a != 0 and not repeatingseq(seqs):
        factor = -1
        while a / b == 0:
            a *= 10
            factor += 1
        seqs.append((a,'0'*factor + str(a / b)))
        a = a % b
    rep = repeatingseq(seqs)
    if rep:
        return [x[1] for x in repeatingseq(seqs)]
    else:
        return []

lens = sorted([(i,findrepeating(1,i)) for i in range(1,1001)],key=lambda x:len(x[1]));

print lens[-1]

