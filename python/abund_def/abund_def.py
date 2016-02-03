#!/usr/bin/python3
import sys

def aliquot_sum(n):
    return sum([d for d in range(1, n//2 + 1) if n % d == 0])


if __name__ == '__main__':
    for line in sys.stdin.readlines():
        n = int(line.strip())
        aqs = aliquot_sum(n)
        if aqs < n:
            print(n, 'is deficient by', n-aqs)
        elif aqs == n:
            print(n, 'is perfect')
        else:
            print(n, 'is abundant by', aqs-n)

