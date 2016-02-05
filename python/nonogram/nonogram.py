#!/usr/bin/python3
from sys import stdin

def desc(ngm):
    return [list(map(len, n.split())) for n in ngm]


def transpose(matrix):
    return [''.join(row) for row in zip(*matrix)]


if __name__ == '__main__':
    rows = [line.rstrip() for line in stdin.readlines()]
    row_desc = desc(rows)

    cols = transpose(rows)
    col_desc = desc(cols)

    max_row_len = max(map(len, row_desc))
    max_col_len = max(map(len, col_desc))

    row_headers = [' '.join(map(str, rd)).rjust(2*max_row_len - 1) for rd in row_desc]
    col_headers = [''.join(map(str, cd)).rjust(max_col_len) for cd in col_desc]

    print('Columns')
    print('=======')
    for tch in transpose(col_headers):
        print(tch)

    print('\nRows')
    print('====')
    for rh in row_headers:
        print(rh)

