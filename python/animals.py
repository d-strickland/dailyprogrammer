#!/usr/bin/python3
import pickle

QUESTION_FILE = 'data/questions.pkl'
ANIMAL_FILE = 'data/animals.pkl'

questions = dict()
animals = set()
remaining = set()

def init():
    with open(QUESTION_FILE, 'rb') as qf, open(ANIMAL_FILE, 'rb') as af:
        questions = pickle.load(qf)
        animals = pickle.load(af)

    remaining = set(animals)  # Start out with the full set and narrow things down.

    print('Welcome to Animal Guess. Please think of an animal.')
    input('Press Enter to continue.\n')


def main():
    init()
    
def get_question():
    pass

def yesorno(prompt):
    response = input(prompt)
    if response.lower() == 'y':
        return 'y'
    if response.lower() == 'n':
        return 'n'
    return yesorno(prompt)

