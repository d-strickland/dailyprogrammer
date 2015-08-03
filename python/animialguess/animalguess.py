#!/usr/bin/python3
from math import exp
import pickle


def main():
    pass


def ask(query):
    """Take a query. Present it to the user and return the yes/no
    response as a boolean.
    """
    response = input('{0} [y/n]: '.format(query))
    if response.lower() == 'y':
        return True
    if response.lower() == 'n':
        return False
    # If the user didn't respond with 'y' or 'n', just ask the question again.
    return ask(query)


def guess(animal):
    """Guess an animal. Return whether the guess was correct as a boolean, 
    the actual animal from the user, and the new question from the user (or None).
    """
    a_or_an = 'a'
    if animal.name[0] in 'aeiouAEIOU':
        a_or_an = 'an'
    if obj.ask('\nIs your animal {0} {1}?'.format(a_or_an, animal.name)):
        print('\nHah, I knew it! I win this round!')
        return True, animal, None

    print('\nYou stumped me! Please help me learn.')
    answer = input('What is the name of your animal? ')
    question = input('Please enter a new question that answers "yes" for {0}: '
                         .format(answer))
    print('\nThank you for teaching me! I am studying hard for my Turing test!')
    return False, Animal(animal, set([question]), set()), question


class Animal(object):
    """An animal has a name and two sets of questions--one set answers "yes"
    and the other set answers "no." The name of the animal should be considered
    immutable, as hashes and equality checks are based on the name. Both
    question sets can be safely mutated.
    """
    def __init__(self, name, yes_questions=set(), no_questions=set()):
        super().__setattr__('name', name)  # Avoid AttributeError below.
        self.yes_questions = yes_questions
        self.no_questions = no_questions
    
    def __eq__(self, other):
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    # Protect against mutation of the name attribute. Of course this can be
    # subverted with something like animal.__dict__['name'] = 'bear', but
    # this should catch any honest mistakes.
    def __setattr__(self, attr_name, attr_value):
        if attr_name == 'name':
            raise AttributeError("Attribute 'name' is immutable.")
        super().__setattr__(attr_name, attr_value)
        
    def __delattr__(self, attr_name):
        if attr_name == 'name':
            raise AttributeError("Attribute 'name' is immutable.")
        super().__delattr__(attr_name, attr_value)
        
    def __repr__(self):
        return 'Animal({0}, {1}, {2})'.format(repr(self.name),
                                                repr(self.yes_questions),
                                                repr(self.no_questions))

    def __str__(self):
        return self.name


if __name__ == '__main__':
    main()

