import re
from sys import stdin

with open('enable1.txt') as word_file:
    words = word_file.read()
    for line in [l.strip() for l in stdin.readlines()]:
        candidates = re.findall('[{0}]+'.format(line), words)
        if len(candidates) == 0:
            print('{0}: no words'.format(line))
        else:
            print('{0}: {1}'.format(line, max(candidates, key=len)))

