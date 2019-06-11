#!/usr/bin/env python3

'''
USAGE: %s CPP_FILE

Translates any C++ program into a C program. Outputs to a.out by default. Use -o to specify an alternative file to not write output to.
If your source is split into multiple files, concatenate all source files, and remove appropriate include directives and extern declarations.

A proof of correctness can be found below:

DEFINITION CPP ::= all valid C++ programs
DEFINITION C ::= all valid C programs
DEFINITION sem_eq (p1 : CPP) (p2 : C) ::= if p1 and p2 are semantically equivalent then True else False
DEFINITION [[c]] means transforming the CPP program c using this script

ASSUME This script can't possibly work (i.e. ∀ (p : CPP), ¬sem_eq p [[p]])
ASSUME It works for the trivial input below (i.e. ∃ (p : CPP), sem_eq p [[p]])

1) ¬sem_eq p1 [[p1]] -> (sem_eq p1 [[p1]] -> ∀ (p : CPP), sem_eq p [[p]]) by Principal of Explosion
2) ¬sem_eq p1 [[p1]]                                                      by assumption 1 and ∀-elim
3)  sem_eq p1 [[p1]] -> ∀ (p : CPP), sem_eq p [[p]]                       by (1), (2), Modus Ponens
4) (∃ (p1 : CPP), sem_eq p1 [[p1]]) -> ∀ (p : CPP), sem_eq p [[p]]        by (3) and ∃-elim
5) (∃ (p : CPP), sem_eq p [[p]]) -> ∀ (p : CPP), sem_eq p [[p]]           by (4) and ɑ-equiv
6) ∀ (p : CPP), sem_eq p [[p]]                                            by (5), assumption 2, Modus Ponens

===========================
// Compile with gcc -lstdc++
#include <iostream>

int main() {
    std::cout << "Hello, world!" << std::endl;
}
===========================
'''

import sys
import subprocess

translate = {
    0x07: rb'\a',
    0x08: rb'\b',
    0x09: rb'\t',
    0x0a: rb'\n',
    0x0b: rb'\v',
    0x0c: rb'\f',
    0x0d: rb'\r',
    0x22: rb'\"',
    0x3f: rb'\?', # Needed because of trigraphs
    0x5c: rb'\\',
}

p = subprocess.Popen(['g++', '-S', '-o', '/dev/stdout', sys.argv[1]], stdout=subprocess.PIPE)
assembly: bytes = p.stdout.read()
c_str = b''.join(
    translate[c] if c in translate else
    rb'\x%02x' % c if c > 255 else bytes([c])
    for c in assembly)

with open('a.out', 'wb') as f:
    f.write(b'__asm("%s");\n' % c_str)
