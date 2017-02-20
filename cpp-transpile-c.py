#!/usr/bin/env python2

import sys
import subprocess

translate = {0x07: '\\a', 0x08: '\\b', 0x0c: '\\f', 0x0a: '\\n', 0x0d: '\\r', 0x09: '\\t',
             0x0b: '\\v', 0x5c: '\\\\', 0x27: "\\'", 0x22: '\\"', 0x3f: '\\?'}

p = subprocess.Popen(['g++', '-S', '-o', '/dev/stdout', sys.argv[1]], stdout=subprocess.PIPE)
assembly = p.stdout.read()

sys.stdout.write('__asm("')
for c in assembly:
    if ord(c) in translate:
        c = translate[ord(c)]
    elif ord(c) > 255:
        c = '\\x' + hex(ord(c))[2:]
    sys.stdout.write(c)
sys.stdout.write('");')
