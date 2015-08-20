#!/usr/bin/python3
import itertools
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
        self.yes_questions = set(yes_questions)
        self.no_questions = set(no_questions)
    
    def __eq__(self, other):
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    # Protect against mutation of the name attribute. Of course this can be
    # subverted with something pathological, like:
    #    animal.__dict__['name'] = 'bear', but
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


class QuestionIterator(object):
    """Iterate through the questions with the goal of narrowing down the list
    of candidates to one as quickly as possible.
    """

    def __init__(self, questions, candidates, ask_func=ask):
        self.questions = set(questions)
        # Start all candidate animals out with a score of zero.
        self.candidate_scores = dict(zip(candidates, itertools.repeat(0)))
        self.ask_func = ask_func

    def __iter__(self):
        return self

    def _yes_animals(self, question):
        """Take a quesiton and return the animals that answer yes to that
        question as a set.
        """
        fltr = lambda x: question in x.yes_questions
        return set(filter(fltr, self.candidate_scores.keys()))

    def _no_animals(self, question):
        """Take a quesiton and return the animals that answer no to that
        question as a set.
        """
        fltr = lambda x: question in x.no_questions
        return set(filter(fltr, self.candidate_scores.keys()))

    def _yes_polarization(self, question):
        """Take a question and return an index of how well the question
        partitions the set of animals, assuming the answer to the quesiton is
        yes. The index will be greater than or equal to zero. Zero represents
        a "worst possible" question, where either all of the candidate animals
        answer yes or all of the candidate animals answer no. The higher the
        index, the closer an answer of "yes" would come to splitting the
        candidate set in half.
        """
        return 0

    def _no_polarization(self, question):
        """Take a question and return an index of how well the question
        partitions the set of animals, assuming the answer to the quesiton is
        yes. The "no" index follows all of the same rules as the "yes" index
        """
        return 0

if __name__ == '__main__':
    main()

