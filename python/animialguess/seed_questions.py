#!/usr/bin/python3
"""Set up some initial data for the animal guessing game."""
import pickle

from animal_guess import Question, QUESTION_FILE, ANIMAL_FILE

questions = {
    Question('Is your animal a mammal?', {'moose', 'kangaroo', 'platypus'}, {'chicken'}),
    Question('Does your animal lay eggs?', {'platypus', 'chicken'}, {'moose', 'kangaroo'}),
    Question('Does your animal walk on two legs?', {'chicken', 'kangaroo'}, {'moose', 'platypus'})
    }

animals = {'moose', 'kangaroo', 'chicken', 'platypus'}

with open(QUESTION_FILE, 'wb') as qf, open(ANIMAL_FILE, 'wb') as af:
    pickle.dump(questions, qf)
    pickle.dump(animals, af)

