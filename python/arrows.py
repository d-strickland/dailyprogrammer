#!/usr/bin/python3
from sys import stdin
import networkx as nx

def to_edges(width, height, arrows):
    """Take the width and height of the grid, and a collection of arrows.
    Return a list of edges in a directed graph represented by the grid of arrows.
    """
    return [_to_edge(width, height, i, a) for (i, a) in enumerate(arrows)]


def _to_edge(w, h, i, a):
    """Take a width, height, index, and an arrow. Return the corresponding edge
    of the digraph as a pair of indices."""
    x = i % w
    y = i // w
    assert x <= w
    assert y <= h

    if a == '>':     # Edge from this node to the right node, or wrap around.
        x = (x+1) % w
    elif a == '<':   # Edge from this node to the left node, or wrap around.
        x = (x-1) % w
    elif a in 'vV':  # Edge from this node to the lower node, or wrap around.
        y = (y+1) % h
    elif a == '^':   # Edge from this node to the upper node, or wrap around.
        y = (y-1) % h
    else:
        raise InputError('"{0}" is not an arrow.'.format(a))
    
    return (i, w*y + x)


if __name__ == '__main__':
    bounds = stdin.readline().split()
    width = int(bounds[0])
    height = int(bounds[1])
    arrows = stdin.read().replace('\n', '').replace('\r', '')

    # Creating the graph and adding the edges will automatically add the nodes.
    graph = nx.DiGraph()
    graph.add_edges_from(to_edges(width, height, arrows))

    cycles = nx.strongly_connected_components(graph)
    max_cycle = max(cycles, key=len)

    if len(max_cycle) == 1:
        print('Grid contains no cycles.')
    else:
        print('Largest cycle is', len(max_cycle), 'nodes:', end='')

    for i, a in enumerate(arrows):
        if i % width == 0:
            print()
        if i in max_cycle:
            print(a, end='')
        else:
            print(' ', end='')
    print()

