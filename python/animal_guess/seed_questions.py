#!/usr/bin/python3
"""Set up some initial data for the animal guessing game."""
import pickle

from animals import QUESTION_FILE, ANIMAL_FILE

questions = {'Is your animal a mammal?': {'moose', 'kangaroo', 'platypus'},
             'Does your animal lay eggs?': {'platypus', 'chicken'},
             'Does your animal walk on two legs?': {'chicken', 'kangaroo'}}

animals = {'moose', 'kangaroo', 'chicken', 'platypus'}

with open(QUESTION_FILE, 'wb') as qf, open(ANIMAL_FILE, 'wb') as af:
    pickle.dump(questions, qf)
    pickle.dump(animals, af)

