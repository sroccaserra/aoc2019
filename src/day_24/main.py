import fileinput
import sys
from collections import defaultdict


def parse(lines):
    m = defaultdict(bool)
    m['w'] = len(lines[0])
    m['h'] = len(lines)
    for y in range(m['h']):
        row = lines[y]
        for x in range(m['w']):
            c = row[x]
            if c == '#':
                m[(x, y)] = True
    return m


def solve_1(grid):
    history = set()
    while True:
        add_to_history(history, grid)
        grid = step(grid)
        if is_in_history(history, grid):
            break
    return biodiversity_rating(grid)


def add_to_history(history, grid):
    cells = sorted_cells(grid)
    history.add(cells)


def is_in_history(history, grid):
    cells = sorted_cells(grid)
    return cells in history


def sorted_cells(grid):
    cells = [(a, b) for (a, b) in grid.items() if (a != 'w' and a != 'h')]
    return tuple(sorted(cells))


def step(grid):
    result = defaultdict(bool)
    result['w'] = grid['w']
    result['h'] = grid['h']
    for y in range(grid['h']):
        for x in range(grid['w']):
            bug = grid[(x, y)]
            ns = neighbors(grid, (x, y))
            n = nb_alive(ns)
            if bug and n == 1:
                result[(x, y)] = True
            if (not bug) and (n == 1 or n == 2):
                result[(x, y)] = True
    return result


def neighbors(grid, cell):
    result = []
    (x, y) = cell
    for (dx, dy) in [(0, -1), (1, 0), (0, 1), (-1, 0)]:
        result.append(grid[(x+dx, y+dy)])
    return result


def nb_alive(ns):
    return len([x for x in ns if x])


def biodiversity_rating(grid):
    result = 0
    for y in range(grid['h']):
        for x in range(grid['w']):
            if grid[(x, y)]:
                result += pow(2, x + grid['w']*y)
    return result


if __name__ == '__main__' and not sys.flags.interactive:
    lines = [line.strip() for line in fileinput.input()]
    grid = parse(lines)
    print(solve_1(grid))
