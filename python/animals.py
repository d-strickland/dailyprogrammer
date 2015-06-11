#!/usr/bin/python3
import pickle

QUESTION_FILE = 'data/questions.pkl'
ANIMAL_FILE = 'data/animals.pkl'

questions = dict()
animals = set()
candidates = set()

def init():
    with open(QUESTION_FILE, 'rb') as qf, open(ANIMAL_FILE, 'rb') as af:
        questions = pickle.load(qf)
        animals = pickle.load(af)

    candidates = set(animals)  # Start out with the full set and narrow things down.

    print('Welcome to Animal Guess. Please think of an animal.')
    input('Press Enter to continue.\n')


def main():
    init()
    
def polarization(q):
    """An index of how well a question partitions the set of candidate animals.

    A lower value represents a better partition. The perfect partition splits the
    candidate set exactly in half, so target that.
    """
    overlap = questions[q].intersection(candidates)
    return abs(len(overlap) - (len(candidates) / 2))

def next_question():
    return min(questions.keys(), key=polarization)

def yesorno(prompt):
    response = input(prompt)
    if response.lower() == 'y':
        return 'y'
    if response.lower() == 'n':
        return 'n'
    return yesorno(prompt)

