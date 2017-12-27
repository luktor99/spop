#!/usr/bin/env python3
import sys

EMPTY = ' '
HOUSE = 'H'
CONT = 'O'

def parse_file(file_in):
    file_out = file_in.replace("output", "input")

    print("*** Parsing file '" + file_in + "' ...")

    # Read board data from the input file
    board = []
    with open(file_in) as fp:
        board = fp.read().splitlines()

    board_transposed = list(map(list, zip(*board)))

    # Count houses in rows
    rows_n = []
    for row in board:
        rows_n.append(row.count(CONT))

    print("x_n = " + str(rows_n))

    # Count houses in cols
    cols_n = []
    for col in board_transposed:
        cols_n.append(col.count(CONT))

    print("y_n = " + str(cols_n))

    # Generate a list of houses' coordinates
    houses = []
    for i, row in enumerate(board):
        indices = [i for i, x in enumerate(row) if x == HOUSE]
        coords = list(zip([i]*len(indices), indices))
        houses.extend(coords)

    print("houses = " + str(houses))

    if sum(rows_n) != sum(cols_n):
        print("Bad sum!")
        exit(0)
    if sum(rows_n) != len(houses):
        print("The number of containers and houses is different!")
        exit(0)

    with open(file_out, 'w') as fp:
        fp.write(str(rows_n) + '\n')
        fp.write(str(cols_n) + '\n')
        fp.write(str(houses) + '\n')

    print("Output written to '" + file_out + "' ...")

files = sys.argv[1:]
for f in files:
    parse_file(f)
