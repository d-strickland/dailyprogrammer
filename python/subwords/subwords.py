#!/usr/bin/python3
from collections import deque

with open('/usr/share/dict/enable1.txt') as word_file:
    WORDS = set(line.strip() for line in word_file.readlines())


def memoize(memoized_func):
    cache = {}
    def wrap(word, *args, **kwargs):
        if not (word in cache):
            cache[word] = memoized_func(word, *args, **kwargs)
        return cache[word]
    return wrap


@memoize
def subwords(word, words=WORDS):
    if word in words:
        return deque([word])
    elif len(word) == 1:
        return None

    for prefix, suffix in ((word[:i], word[i:])
            for i in range(len(word)-1, 0, -1) if word[:i] in words):
        tail = subwords(suffix)
        if tail == None:
            continue
        else:
            tail.appendleft(prefix)
            return tail
    return None


if __name__ == '__main__':
    print(subwords('applepie'))

