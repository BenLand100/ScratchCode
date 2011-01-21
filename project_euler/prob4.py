#!/usr/bin/python

print filter(lambda x: x == x[::-1], map(str,sorted([i*j for i in range(0,1000) for j in range(0,1000)])))[-1]
