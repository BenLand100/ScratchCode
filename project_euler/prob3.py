#!/usr/bin/python

num = 600851475143
factor = 2

while num > 1:
    while num % factor == 0:
        print factor
        num /= factor
    factor += 1
   
