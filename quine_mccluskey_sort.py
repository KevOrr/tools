#!/usr/bin/env python3

import sys

nums = []
for line in sys.stdin:
    if 'end' in line.lower():
        break
    nums.append(int(line.strip()))

for num in sorted(nums, key=lambda n: bin(n)[2:].count('1')):
    print('{}: {}'.format(num, bin(num)[2:]))
