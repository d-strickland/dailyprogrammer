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
        
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.assertEmpty = self.assertFalse

    def testEmptyAnimal(self):
        animal = ag.Animal("")
        self.assertEmpty(animal.name)
        self.assertEmpty(animal.yes_questions)
        self.assertEmpty(animal.no_questions)

    def testUsefulAnimal(self):
        a = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        self.assertEqual('mouse', a.name)
        self.assertSetEqual(set(self.questions[:3]), a.yes_questions)
        self.assertSetEqual(set([self.questions[3]]), a.no_questions)

    def testEqual1(self):
        a1 = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        a2 = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        self.assertEqual(hash(a1), hash(a2))
        self.assertEqual(a1, a2)

    def testEqual2(self):
        a1 = ag.Animal('mouse', set(self.questions[:2]), set())
        a2 = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        self.assertEqual(hash(a1), hash(a2))
        self.assertEqual(a1, a2)

    def testInequal(self):
        a1 = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        a2 = ag.Animal('bear', set(self.questions[:3]), set([self.questions[3]]))
        self.assertNotEqual(hash(a1), hash(a2))
        self.assertNotEqual(a1, a2)

    def testCantChangeName(self):
        a = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        with self.assertRaises(AttributeError):
            a.name = 'bear'

    def testCantDeleteName(self):
        a = ag.Animal('mouse', set(self.questions[:3]), set([self.questions[3]]))
        with self.assertRaises(AttributeError):
            del a.name

    def testRepr(self):
        yes = set(self.questions[:3])
        no = set([self.questions[3]])
        a = ag.Animal('mouse', yes, no)
        self.assertEqual("Animal('mouse', {0}, {1})".format(repr(yes), repr(no)),
                            repr(a))

    def testStr(self):
        yes = set(self.questions[:3])
        no = set([self.questions[3]])
        a = ag.Animal('mouse', yes, no)
        self.assertEqual('mouse', str(a))

