#!/usr/bin/python


def calcpaths(sz):
    grid = [[1 for c in range(0,sz+1)] for c in range(0,sz+1)]
    for r in range(sz-1,-1,-1):
        for c in range(sz-1,-1,-1):
            grid[r][c] = grid[r+1][c] + grid[r][c+1]
    return grid[0][0]
        
paths = calcpaths(20)
print 'paths:',paths
