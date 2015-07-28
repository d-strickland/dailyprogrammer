#!/usr/bin/python3
import unittest

import animal_guess_old as ag

class AnimalGuessTestCase(unittest.TestCase):
    """Base class for animal guess unit tests."""

    def setUp(self):
        self.mammals = {'aardvark', 'pangolin', 'kangaroo rat'}
        self.non_mammals = {'cobra', 'alligator gar', 'paddlefish'}
    
    def testFirst(self):
        self.assertEqual(1, ag.first({1}))


class QuestionTestCase(AnimalGuessTestCase):
    """Tests for animal questions"""

    def testEmptyQuestion(self):
        q = ag.Question("")
        self.assertFalse(q.question)
        self.assertFalse(q.yes_animals)
        self.assertFalse(q.no_animals)
    
    def testInterestingQuestion(self):
        q = ag.Question("Is your animal a mammal?", self.mammals, self.non_mammals)
        self.assertSetEqual(self.mammals, q.yes_animals)
        self.assertSetEqual(self.non_mammals, q.no_animals)

    def testEqualQuestions(self):
        mammals2 = {'pangolin', 'aardvark', 'kangaroo rat'}
        non_mammals2 = {'alligator gar', 'cobra', 'paddlefish'}
        p = ag.Question("Is your animal a mammal?", self.mammals, self.non_mammals)
        q = ag.Question("Is your animal a mammal?", mammals2, non_mammals2)
        self.assertEqual(p, q)

    def testEqualQuestions2(self):
        mammals2 = {'pangolin', 'aardvark', 'kangaroo rat'}
        non_mammals2 = {'cobra', 'paddlefish'}
        p = ag.Question("Is your animal a mammal?", self.mammals, self.non_mammals)
        q = ag.Question("Is your animal a mammal?", mammals2, non_mammals2)
        self.assertEqual(p, q)


class MockParser(object):
    def __init__(self, answers):
        self.answers = answers

    def ask(self, question):
        return self.answers.pop()


class PolarizationTestCase(AnimalGuessTestCase):
    """Tests for the polarization function."""

    def setUp(self):
        super().setUp()
        self.candidates = self.mammals.union(self.non_mammals)

    def testTooGeneral(self):
        q = ag.Question("Is your animal an animal?", self.candidates, set())
        qi = ag.QuestionIterator([q], self.candidates, MockParser([]))
        self.assertEqual(0, qi.polarization(q))

    def testTooSpecific(self):
        q = ag.Question("Does your animal bury its head in the sand?", set(),
                            self.candidates)
        qi = ag.QuestionIterator([q], self.candidates, MockParser([]))
        self.assertEqual(0, qi.polarization(q))

    def testPerfect(self):
        q = ag.Question("Is your animal a mammal?", self.mammals, self.non_mammals)
        qi = ag.QuestionIterator([q], self.candidates, MockParser([]))
        self.assertEqual(1.001, qi.polarization(q))

    def testComparison1(self):
        p = ag.Question("Huh?", {'pangolin', 'aardvark'}, {'cobra', 'paddlefish'})
        q = ag.Question("Eh?", {'pangolin'}, {'cobra', 'paddlefish'})
        qi = ag.QuestionIterator([p, q], self.candidates, MockParser([]))
        p_index = qi.polarization(p)
        q_index = qi.polarization(q)

        self.assertGreater(1, p_index)
        self.assertGreater(1, q_index)
        self.assertLess(0, p_index)
        self.assertLess(0, q_index)

        self.assertGreater(p_index, q_index)

    def testComparison2(self):
        p = ag.Question("Huh?", {'pangolin', 'aardvark'},
                                {'cobra', 'paddlefish', 'kangaroo rat'})
        q = ag.Question("Eh?", {'pangolin', 'aardvark'}, {'cobra', 'paddlefish'})
        qi = ag.QuestionIterator([p, q], self.candidates, MockParser([]))
        p_index = qi.polarization(p)
        q_index = qi.polarization(q)

        self.assertGreater(1, p_index)
        self.assertGreater(1, q_index)
        self.assertLess(0, p_index)
        self.assertLess(0, q_index)

        self.assertGreater(p_index, q_index)


class QuestionIteratorTestCase(AnimalGuessTestCase):

    def setUp(self):
        super().setUp()
        self.candidates = self.mammals.union(self.non_mammals)
        self.mammal_q = ag.Question("Is your animal a mammal?", self.mammals,
                                        self.non_mammals)

    def testNext(self):
        p = ag.Question("Huh?", {'pangolin', 'aardvark'},
                                {'cobra', 'paddlefish', 'kangaroo rat'})
        mp = MockParser([False, True, False])
        qi = ag.QuestionIterator([p, self.mammal_q], self.candidates, mp)
        self.assertTupleEqual((self.mammal_q, False, self.non_mammals), next(qi))

    def testDownToOne(self):
        qi = ag.QuestionIterator([self.mammal_q], {'aardvark'}, MockParser([]))
        self.assertRaises(StopIteration, next, qi)

    def testNoUseful(self):
        qi = ag.QuestionIterator([self.mammal_q], self.mammals, MockParser([]))
        self.assertRaises(StopIteration, next, qi)

    def testNoQuestions(self):
        qi = ag.QuestionIterator([], self.mammals, MockParser([]))
        self.assertRaises(StopIteration, next, qi)
        

if __name__ == '__main__':
    unittest.main()

