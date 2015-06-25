#!/usr/bin/python3
import pickle

QUESTION_FILE = 'data/questions.pkl'
ANIMAL_FILE = 'data/animals.pkl'

questions = dict()  # eg. {'Is your animal a mammal?': {'moose', 'kangaroo', 'platypus'},
                    #      'Does your animal lay eggs?': {'platypus', 'chicken'},
                    #      'Does your animal have two legs?': {'chicken', 'kangaroo'}}

animals = set()     # eg. {'moose', 'kangaroo', 'chicken', 'platypus'}
candidates = set()


def main():
    setup()
    correct_animal, yes_questions = play()
    teardown(correct_animal, yes_questions)


def setup():
    global questions, animals, candidates
    with open(QUESTION_FILE, 'rb') as qf, open(ANIMAL_FILE, 'rb') as af:
        questions = pickle.load(qf)
        animals = pickle.load(af)

    candidates = set(animals)  # Start out with the full set and narrow things down.

    print('Welcome to Animal Guess! Please think of an animal.')
    raw_input('Press Enter to continue.\n')


def play():
    """Play a round of the game.

    Return the correct animal and the questions that the user responded "yes" to.
    """
    global questions, animals, candidates
    yes_questions = []

    while len(candidates) > 1:
        q = next_question()

        if candidates.issubset(questions[q]) or len(candidates.intersection(questions[q])) == 0:
            # Couldn't narrow it down any with our given information. Just make
            # an arbitrary guess.
            return guess(next(iter(candidates)), yes_questions)

        if yes(q):
            yes_questions.append(q)
            candidates = candidates.intersection(questions[q])
        else:
            candidates = candidates.difference(questions[q])

    assert len(candidates) == 1
    return guess(next(iter(candidates)), yes_questions)


def guess(animal, yes_questions):
    """Guess an animal. Return whether the guess was correct as a boolean, 
    the actual animal from the user, and the new question from the user.
    """
    a_or_an = 'a'
    if animal[0] in 'aeiouAEIOU':
        a_or_an = 'an'
    if yes('\nIs your animal {0} {1}?'.format(a_or_an, animal)):
        print('\nHah, I knew it! I win this round!')
        return animal, yes_questions
    print('\nYou stumped me! Please help me learn.')
    answer = raw_input('What is the name of your animal? ')
    question = raw_input('Please enter a new question that answers "yes" for {0}: '.format(answer))
    print('\nThank you for teaching me! I am studying hard for my Turing test!')
    return answer, [question] + yes_questions


def teardown(animal, yes_questions):
    """Take the correct animal and the list of known questions that answer yes.

    Update our data with this new information and write it back to the files.
    """
    global questions, animals
    animals.add(animal)
    for q in yes_questions:
        if q in questions:
            questions[q].add(animal)
        else:
            questions[q] = {animal}

    with open(QUESTION_FILE, 'wb') as qf, open(ANIMAL_FILE, 'wb') as af:
        pickle.dump(questions, qf)
        pickle.dump(animals, af)


def polarization(q):
    """An index of how well a question partitions the set of candidate animals.

    A lower value represents a better partition. A "perfect" partition splits the
    candidate set exactly in half, so target that.
    """
    overlap = questions[q].intersection(candidates)
    return abs(len(overlap) - (len(candidates) / 2))


def next_question():
    return min(questions.keys(), key=polarization)


def yes(prompt):
    """Ask a yes or no question and return the answer as a boolean."""
    response = raw_input('{0} [y/n]: '.format(prompt))
    if response.lower() == 'y':
        return True
    if response.lower() == 'n':
        return False
    return yes(prompt)


if __name__ == '__main__':
    main()

