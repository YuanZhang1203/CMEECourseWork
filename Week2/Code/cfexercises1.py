#!/usr/bin/env python3

"""Some functions exemplifying the use of control statements"""
#docstrings are considered part of the running code (normal comments are
#stripped). Hence, you can access your docstrings at run time.
__author__ = 'Yuan Zhang (yz12119@ic.ac.uk)'
__version__ = '0.0.1'

import sys

def foo_1(x):
    return "The square root of %d is %d!" % (x, x ** 0.5)

def foo_2(x, y):
    """Find which is the larger x among x,y"""
    if x > y: 
        return "The larger one is %d!" % x 
    return "The larger one is %d!" % y

def foo_3(x, y, z):
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return "The new sequence is %d,%d,%d!" % (x, y, z)

def foo_4(x):
    res = 1
    for i in range(1, x + 1):
        res = res * i
    return "The result is %d!" % res

def foo_5(x): # a recursive function that calculates the factorial of x
    if x == 1:
        print("The factorial result is 1")
        return 1
    return "The factorial result is %d!" % (x * (x - 1))

def foo_6(x): # Calculate the factorial of x in a different way
    facto = 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return "The anthor factorial result is %d!" % facto

def main(argv):
    print(foo_1(10))
    print(foo_2(10, 4))
    print(foo_3(10, 12, 5))
    print(foo_4(10))
    print(foo_5(10))
    print(foo_6(10))
    return 0

if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)