#!/usr/bin/python

def divisible(x,vals):
    for i in vals:
        if x % i != 0:
            return 0
    return 1

i = 20
num = 0
facts = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
while num == 0:
    if divisible(i,facts):
        num = i
    i += 20

print num
