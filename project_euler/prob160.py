#!/usr/bin/python

def spc_factorial(x,modmask):
    "computes (x! % modmask)"
    print 'factorial:',x
    res = 1
    while x > 0:
        if x % 1000 == 0: print x
        c = x
        while c % 10 == 0:
            c /= 10
        res = c * res
        while res % 10 == 0:
            res /= 10
        x -= 1
    return res % modmask

def factorial(x):
    print 'factorial:',x
    res = 1
    while x > 1:
        if x % 1000 == 0: print x
        res *= x
        x -= 1
    return res

def opt_exp(b,e,modmask):
    "computes (b**e % modmask) using a repeating optimization"
    res = 1
    if e / modmask > 0:
        for i in range(0,modmask):
            res = (res * b) % modmask
    b %= modmask
    for i in range(0,e%modmask):
        if i != 0 and i % modmask == 0: print b,e,res
        res = (res * b) % modmask
    return res % modmask

def leastsig(x,n):
    while x % 10 == 0:
        x /= 10
    return x%(10**n)


def opt_spc_factorial(x,modmask):
    "computes (x! % modmask) using a repeating optimization"
    exp = x / modmask
    res = x % modmask
    return ((factorial(res)%modmask)*opt_exp((leastsig(factorial(modmask))%modmask),exp,modmask)) % modmask


print opt_spc_factorial(1000000000,10**5)

#mod = 10000
#for i in range(1,1000):
#    a = leastsig(factorial(i),10)%mod
#    b = spc_factorial(i,mod)
#    if a != b:
#        print i,a,b
#        break
