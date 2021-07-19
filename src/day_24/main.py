import fileinput
import sys
from collections import defaultdict

W = 5
H = 5


def parse(lines):
    cells = set()
    for y in range(H):
        row = lines[y]
        for x in range(W):
            c = row[x]
            if c == '#':
                cells.add((x, y))
    return cells


def solve_1(cells):
    history = set()
    while True:
        add_to_history(history, cells)
        cells = step(cells)
        if is_in_history(history, cells):
            break
    return biodiversity_rating(cells)


def add_to_history(history, cells):
    cell_items = sorted_items(cells)
    history.add(cell_items)


def is_in_history(history, cells):
    cell_items = sorted_items(cells)
    return cell_items in history


def sorted_items(cells):
    return tuple(sorted(cells))


def step(cells):
    result = set()
    for y in range(H):
        for x in range(W):
            pos = (x, y)
            bug = pos in cells
            ns = neighbors(cells, pos)
            n = nb_alive(ns)
            if bug and n == 1:
                result.add(pos)
            if (not bug) and (n == 1 or n == 2):
                result.add(pos)
    return result


def neighbors(cells, pos):
    x, y = pos
    result = []
    for dx, dy in [(0, -1), (1, 0), (0, 1), (-1, 0)]:
        result.append((x + dx, y + dy) in cells)
    return result


def nb_alive(ns):
    return len([x for x in ns if x])


def biodiversity_rating(cells):
    result = 0
    for y in range(H):
        for x in range(W):
            if (x, y) in cells:
                result += pow(2, x + W*y)
    return result


##
# Part two

def solve_2(initial_cells):
    cells = defaultdict(bool)
    for (x, y) in initial_cells:
        cells[(x, y, 0)] = True
    print(show(cells, 0))
    for depth in range(10):
        cells = step_folded(depth, cells)
        print(show(cells, depth+1))
    return None


def show(cells, depth):
    levels = []
    for d in range(-depth, depth+1):
        lines = []
        for y in range(H):
            line = []
            for x in range(W):
                bug = cells[(x, y, d)]
                c = '#' if bug else '.'
                if (x, y) == (2, 2):
                    line.append('?')
                else:
                    line.append(c)
            lines.append(''.join(line))
        levels.append('{}\n'.format(d)+'\n'.join(lines))
    return '\n\n'.join(levels)


def step_folded(depth, cells):
    result = defaultdict(bool)
    for d in range(-depth - 1, depth+2):
        for y in range(H):
            for x in range(W):
                if x == 2 and y == 2:
                    continue
                pos = (x, y, d)
                bug = cells[pos]
                ns = neighbors_folded(cells, pos)
                n = nb_alive(ns)
                if bug and n == 1:
                    result[pos] = True
                if (not bug) and (n == 1 or n == 2):
                    result[pos] = True
    return result


def neighbors_folded(cells, pos):
    x, y, _ = pos

    result = []
    for dx, dy in [(0, -1), (1, 0), (0, 1), (-1, 0)]:
        result.append(cells[(x+dx, y+dy)])

    return result + \
        neighbors_folded_outer(cells, pos) + \
        neighbors_folded_inner(cells, pos)


def neighbors_folded_outer(cells, pos):
    x, y, d = pos
    if x == 0:
        return [cells[(1, 2, d-1)]]
    elif x == W-1:
        return [cells[(3, 2, d-1)]]
    elif y == 0:
        return [cells[(2, 2, d-1)]]
    elif y == H-1:
        return [cells[(2, 2, d-1)]]
    else:
        return []


def neighbors_folded_inner(cells, pos):
    x, y, d = pos
    if x == 2 and y == 1:
        return [cells[(i, 0, d+1)] for i in range(W)]
    elif x == 2 and y == 3:
        return [cells[(i, H-1, d+1)] for i in range(W)]
    elif y == 2 and x == 1:
        return [cells[(0, j, d+1)] for j in range(H)]
    elif y == 2 and x == 3:
        return [cells[(W-1, j, d+1)] for j in range(H)]
    else:
        return []


if __name__ == '__main__' and not sys.flags.interactive:
    lines = [line.strip() for line in fileinput.input()]
    cells = parse(lines)
    print(solve_1(cells.copy()))
    print(solve_2(cells))
