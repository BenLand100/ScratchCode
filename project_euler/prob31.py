#!/usr/bin/python

coins = [1,2,5,10,20,50,100,200]
amt = 200

def consume(amt,values):
    if len(values) == 0: return amt == 0
    ways = 0
    for i in range(0,amt/values[0]+1):
        ways += consume(amt-values[0]*i,values[1:])
    return ways

print consume(amt,coins)
        
