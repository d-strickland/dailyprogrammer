#!/usr/bin/python3
from math import exp
import pickle

QUESTION_FILE = 'data/questions.pkl'
ANIMAL_FILE = 'data/animals.pkl'


def main():
    questions, candidates, parser = setup()
    yes_questions = []
    no_questions = []

    remaining = set(candidates)
    for question, answer, remaining in QuestionIterator(questions, candidates, parser):
        if answer:
            yes_questions.append(question)
        else:
            no_questions.append(question)

    animal, yes_questions = parser.guess(first(remaining), yes_questions)

    teardown(animal, questions, candidates, yes_questions, no_questions)


def setup():
    print('Welcome to Animal Guess! Please think of an animal.')
    input('Press Enter to continue.\n')
    with open(QUESTION_FILE, 'rb') as qf, open(ANIMAL_FILE, 'rb') as af:
        return pickle.load(qf), pickle.load(af), Parser()


def teardown(animal, questions, candidates, yes_questions, no_questions):
    """Take the correct animal, the questions we started with, the candidates we
    started with, and the yes and no answers that we got from the user.

    Update our data with this new information and write it back to the files.
    """
    new_questions = set(questions)
    for q in yes_questions:
        new_questions.add(Question(q.question, q.yes_animals.union(set([animal])), q.no_animals))

    for q in no_questions:
        new_questions.add(Question(q.question, q.yes_animals, q.no_animals.union(set([animal]))))

    with open(QUESTION_FILE, 'wb') as qf, open(ANIMAL_FILE, 'wb') as af:
        pickle.dump(new_questions, qf)
        pickle.dump(candidates.union(set([animal])), af)


def first(collection):
    """Return the first item of a collection (or an indeterminate element of a set)."""
    return next(iter(collection))


class Question(object):
    def __init__(self, question, yes_animals=set(), no_animals=set()):
        self.question = question
        self.yes_animals = yes_animals
        self.no_animals = no_animals
    
    def __key(self):
        return self.question

    def __eq__(self, other):
        return self.__key() == other.__key()

    def __hash__(self):
        return hash(self.__key())


class QuestionIterator(object):
    """Iterate through the questions with the goal of narrowing down the list
    of candidates to one as quickly as possible.
    """

    def __init__(self, questions, candidates, parser):
        self.questions = list(questions)
        self.candidates = set(candidates)
        self.parser = parser

    def __iter__(self):
        return self

    def __next__(self):
        """Ask the next question that partitions the candidate list well. Remove
        that question from the list and update the candidate animals based on the
        answer. Return the question asked, the answer given, and the updated
        candidate list.
        """
        if not self.questions:
            raise StopIteration()
        
        if len(self.candidates) == 1:
            raise StopIteration()

        q = max(self.questions, key=self.polarization)

        if self.polarization(q) == 0:
            # None of the questions narrowed down the candidates list, so stop
            # iteration.
            raise StopIteration()

        self.questions.remove(q)

        answer = self.parser.ask(q)
        if answer:
            self.candidates.difference_update(q.no_animals)
        else:
            self.candidates.difference_update(q.yes_animals)
        if not self.candidates:
            raise StopIteration()

        return q, answer, self.candidates

    def polarization(self, q):
        """Take a question and a set of candidate animals. Return an index of how
        good the question is, where a higher index represents a better question.

        This is essentially the scoring function for a minimax algorithm, but we are
        pruning the tree after one level. Since we will narrow down the candidate
        list based on either the "yes" animals and the "no" animals, depending on
        the answer to the question, we will check how each answer partitions the
        results and base our index primarily on the worst-case answer. The best-case
        answer also contributes to the index, but gets a much lesser weight.

        The best conceivable question splits the candidate list exactly in half, and
        has a polarization index of 1.001. The worst conceivable question does not
        narrow down the candidate list at all, and has an index of zero.
        """
        num_candidates = len(self.candidates)
        yes_intersect = len(self.candidates.intersection(q.yes_animals))
        no_intersect = len(self.candidates.intersection(q.no_animals))

        yes_index = 0
        no_index = 0

        # Use a normal distribution with a mean of len(c)/2 for each index. This
        # will peak at a value of 1 when the answer splits the candidate list
        # exactly in half, and steadily decrease for less ideal questions. There's
        # no need to normalize the index, as we're not making a real probability
        # distribution--just comparing relative values.
        # If an answer will not narrow down the candidate list at all, leave the
        # index set to zero.
        if (yes_intersect != 0) and (yes_intersect != num_candidates):
            yes_index = exp(-(yes_intersect - (num_candidates/2))**2)
        if (no_intersect != 0) and (no_intersect != num_candidates):
            no_index = exp(-(no_intersect - (num_candidates/2))**2)

        if yes_index <= no_index:
            return yes_index + (no_index/1000)
        else:
            return no_index + (yes_index/1000)


class Parser(object):
    @classmethod
    def prompt(obj, query):
        """Take a query. Present it to the user and return the yes/no
        response as a boolean.
        """
        response = input('{0} [y/n]: '.format(query))
        if response.lower() == 'y':
            return True
        if response.lower() == 'n':
            return False
        return yes(question)

    @classmethod
    def ask(obj, question):
        return obj.prompt(question.question)

    @classmethod
    def guess(obj, animal, yes_questions):
        """Guess an animal. Return whether the guess was correct as a boolean, 
        the actual animal from the user, and the new question from the user.
        """
        a_or_an = 'a'
        if animal[0] in 'aeiouAEIOU':
            a_or_an = 'an'
        if obj.prompt('\nIs your animal {0} {1}?'.format(a_or_an, animal)):
            print('\nHah, I knew it! I win this round!')
            return animal, yes_questions
        print('\nYou stumped me! Please help me learn.')
        answer = input('What is the name of your animal? ')
        question = input('Please enter a new question that answers "yes" for {0}: '
                             .format(answer))
        print('\nThank you for teaching me! I am studying hard for my Turing test!')
        return answer, [Question(question, set([answer]), set())] + yes_questions


if __name__ == '__main__':
    main()

