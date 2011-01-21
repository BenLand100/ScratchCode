#!/usr/bin/python

from numpy import *

def spiraliter(size):
    half = size / 2
    x,y = half,half
    while 1: 
        yield (x,y)
        rel = min(x,y,size-x-1,size-y-1)
        if x == size-rel-1 and y == rel:
            x+=1
        elif x == size-rel-1 and y == size-rel-1:
            x-=1
        elif x == rel and y == size-rel-1:
            y-=1
        elif x == rel and y == rel:
            x+=1
        elif x == rel:
            y-=1
        elif y == rel:
            x+=1
        elif x == size-rel-1:
            y+=1
        elif y == size-rel-1:
            x-=1

def makespiral(size):
    i = 1
    spiral = spiraliter(size)
    nums = [[0 for x in range(size)] for y in range(size)]
    while i <= size*size:
        pos = spiral.next()
        nums[pos[1]][pos[0]] = i
        i += 1
    return array(nums)    

nums = makespiral(1001)
print sum(diagonal(nums)) + sum(diagonal(nums[:,::-1])) - 1
