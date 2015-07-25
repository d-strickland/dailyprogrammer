#!/usr/bin/python3

class Question(object):
    def __init__(self, question, yes_animals=set(), no_animals=set()):
        self.question = question
        self.yes_animals = yes_animals
        self.no_animals = no_animals
    
    def __eq__(self, other):
        return (self.question == other.question) \
                and (self.yes_animals == other.yes_animals) \
                and (self.no_animals == other.no_animals)

def polarization(question, candidates):
    """Take a question and a set of candidate animals. Return an index of how
    good the question is, where a lower index represents a better question.

    This is essentially the scoring function for a minimax algorithm. Since we
    can narrow down the candidate list based on both the animals we know answer
    "yes" and the animals we know answer "no," we will check how each answer
    partitions the results and base our index primarily on the worst-case answer.
    The best-case answer also contributes to the index, but gets a lesser weight.

    The best conceivable question splits the candidate list exactly in half, and
    has an index of zero or less. The worst conceivable question does not narrow
    down the candidate list at all, and has an index of len(candidates) / 2.
    """
    return 0
