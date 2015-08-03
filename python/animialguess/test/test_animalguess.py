#!/usr/bin/python3
import unittest

import animalguess as ag

class AnimalTestCase(unittest.TestCase):
    questions = [
        'Is your animal a mammal?',
        'Does your animal walk on four legs?',
        'Does your animal have claws?',
        'Does your animal live in the ocean?'
    ]

    mouse = ag.Animal('mouse', set(questions[:3]), set([questions[3]]))
    bear = ag.Animal('bear', set(questions[:3]), set([questions[3]]))
        
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

    def testRepr(self):
        expected = "Animal('mouse', {0}, {1})".format(
            repr(set(self.questions[:3])),
            repr(set([self.questions[3]]))
        )
        self.assertEqual(expected, repr(self.mouse))

    def testStr(self):
        self.assertEqual('mouse', str(self.mouse))

