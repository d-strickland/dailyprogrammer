#!/usr/bin/python3
import unittest

import animal_guess as ag

class AnimalGuessTestCase(unittest.TestCase):
    """Base class for animal guess unit tests."""

    def setUp(self):
        self.mammals = {'aardvark', 'pangolin', 'kangaroo rat'}
        self.non_mammals = {'cobra', 'alligator gar', 'paddlefish'}


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

    def testInequalQuestions(self):
        mammals2 = {'pangolin', 'aardvark', 'kangaroo rat'}
        non_mammals2 = {'cobra', 'paddlefish'}
        p = ag.Question("Is your animal a mammal?", self.mammals, self.non_mammals)
        q = ag.Question("Is your animal a mammal?", mammals2, non_mammals2)
        self.assertNotEqual(p, q)


class PolarizationTestCase(AnimalGuessTestCase):
    """Tests for the polarization function."""

    def setUp(self):
        super().setUp()
        self.candidates = self.mammals.union(self.non_mammals)

    def testTooGeneral(self):
        q = ag.Question("Is your animal an animal?", self.candidates, set())
        self.assertEqual(3, ag.polarization(q, self.candidates))

if __name__ == '__main__':
    unittest.main()

