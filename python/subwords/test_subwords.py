from collections import deque
import unittest

from subwords import subwords, WORDS

class SWTestCase(unittest.TestCase):
    
    scary_words = set(WORDS)

    for i in range(1,1001):
        scary_words.add('a'*i)

    def testFullWord(self):
        self.assertSequenceEqual(deque(['apple']), subwords('apple'))

    def testTwoWords(self):
        self.assertSequenceEqual(deque(['apple', 'pie']), subwords('applepie'))

    def testSearchMiss(self):
        self.assertIsNone(subwords('applez'))

    def testSlow(self):
        self.assertIsNone(subwords('a'*400 + 'z', self.scary_words))

    def testMany(self):
        self.assertSequenceEqual(['this','string','has','lots','of','words','rock','banana','fish'],
                                    subwords('thisstringhaslotsofwordsrockbananafish'))

