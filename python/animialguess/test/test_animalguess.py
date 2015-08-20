#1!/usr/bin/python3
import itertools
import unittest

import animalguess as ag

class AGTestCase(unittest.TestCase):
    questions = [
        'Is your animal a mammal?',
        'Does your animal walk on four legs?',
        'Does your animal have claws?',
        'Does your animal live in the ocean?'
    ]

    mouse = ag.Animal('mouse', questions[:3], [questions[3]])
    bear = ag.Animal('bear', questions[:3], [questions[3]])
    dolphin = ag.Animal('dolphin', [questions[0], questions[3]], questions[1:2])
    moose = ag.Animal('moose', questions[0:1], questions[2:3])

    animals = [mouse, bear, dolphin, moose]


class AnimalTestCase(AGTestCase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.assertEmpty = self.assertFalse

    def testEmptyAnimal(self):
        animal = ag.Animal("")
        self.assertEmpty(animal.name)
        self.assertEmpty(animal.yes_questions)
        self.assertEmpty(animal.no_questions)

    def testUsefulAnimal(self):
        self.assertEqual('mouse', self.mouse.name)
        self.assertSetEqual(set(self.questions[:3]), self.mouse.yes_questions)
        self.assertSetEqual(set([self.questions[3]]), self.mouse.no_questions)

    def testEqual1(self):
        a = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        self.assertEqual(hash(self.mouse), hash(a))
        self.assertEqual(self.mouse, a)

    def testEqual2(self):
        a = ag.Animal('mouse', set(self.questions[:2]), set())
        self.assertEqual(hash(self.mouse), hash(a))
        self.assertEqual(self.mouse, a)

    def testInequal(self):
        self.assertNotEqual(hash(self.mouse), hash(self.bear))
        self.assertNotEqual(self.mouse, self.bear)

    def testCantChangeName(self):
        with self.assertRaises(AttributeError):
            self.mouse.name = 'bear'

    def testCantDeleteName(self):
        with self.assertRaises(AttributeError):
            del self.mouse.name

    def testStr(self):
        self.assertEqual('mouse', str(self.mouse))


class QuestionIteratorTestCase(AGTestCase):
    @classmethod
    def setUpClass(cls):
        mock_ask = lambda obj, prompt: True
        cls.qi = ag.QuestionIterator(cls.questions, cls.animals, mock_ask)

    def testInit(self):
        animal_scores = dict(zip(self.animals, itertools.repeat(0)))
        self.assertSetEqual(set(self.questions), self.qi.questions)
        self.assertDictEqual(animal_scores, self.qi.candidate_scores)

    def testYes(self):
        self.assertSetEqual(set(self.animals), self.qi._yes_animals('Is your animal a mammal?'))
        self.assertSetEqual(set([self.mouse, self.bear, self.moose]), self.qi._yes_animals('Does your animal have claws?'))

    def testNo(self):
        self.assertSetEqual(set(), self.qi._no_animals('Is your animal a mammal?'))
        self.assertSetEqual(set([self.dolphin]), self.qi._no_animals('Does your animal have claws?'))

    def testYesIndex(self):
        """
        """


class PolarizationTestCase(QuestionIteratorTestCase):
    """Tests for the polarization functions."""

    def testTooGeneral(self):
        q = 'Is your animal a mammal?'
        self.assertEqual(0, self.qi._yes_polarization(q))

    def testTooSpecific(self):
        q = 'Is your animal a mammal?'
        self.assertEqual(0, self.qi._no_polarization(q))

    def testComparison1(self):
        pass

    def testComparison2(self):
        pass

