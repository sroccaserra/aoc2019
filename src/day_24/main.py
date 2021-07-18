import fileinput
import sys
from collections import defaultdict

W = 5
H = 5


def parse(lines):
    cells = defaultdict(bool)
    w = len(lines[0])
    h = len(lines)
    for y in range(h):
        row = lines[y]
        for x in range(w):
            c = row[x]
            if c == '#':
                cells[(x, y)] = True
    return w, h, cells


def solve_1(w, h, cells):
    history = set()
    while True:
        add_to_history(history, cells)
        cells = step(w, h, cells)
        if is_in_history(history, cells):
            break
    return biodiversity_rating(w, h, cells)


def add_to_history(history, cells):
    cell_items = sorted_items(cells)
    history.add(cell_items)


def is_in_history(history, cells):
    cell_items = sorted_items(cells)
    return cell_items in history


def sorted_items(cells):
    return tuple(sorted(cells.items()))


def step(w, h, cells):
    result = defaultdict(bool)
    for y in range(h):
        for x in range(w):
            pos = (x, y)
            bug = cells[pos]
            ns = neighbors(cells, pos)
            n = nb_alive(ns)
            if bug and n == 1:
                result[pos] = True
            if (not bug) and (n == 1 or n == 2):
                result[pos] = True
    return result


def neighbors(cells, pos):
    x, y = pos
    result = []
    for dx, dy in [(0, -1), (1, 0), (0, 1), (-1, 0)]:
        result.append(cells[(x+dx, y+dy)])
    return result


def nb_alive(ns):
    return len([x for x in ns if x])


def biodiversity_rating(w, h, cells):
    result = 0
    for y in range(h):
        for x in range(w):
            if cells[(x, y)]:
                result += pow(2, x + w*y)
    return result


##
# Part two

def solve_2(w, h, initial_cells):
    cells = defaultdict(bool)
    for (x, y), bug in initial_cells.items():
        cells[(x, y, 0)] = bug
    print(show(cells, 0))
    for depth in range(1):
        cells = step_folded(w, h, depth, cells)
    return show(cells, depth)


def show(cells, depth):
    levels = []
    for d in range(-depth, depth+1):
        lines = []
        for y in range(h):
            line = []
            for x in range(w):
                bug = cells[(x, y, d)]
                c = '#' if bug else '.'
                line.append(c)
            lines.append(''.join(line))
        levels.append('{}\n'.format(d)+'\n'.join(lines))
    return '\n\n'.join(levels)


def step_folded(w, h, depth, cells):
    result = defaultdict(bool)
    for d in range(-depth, depth+1):
        for y in range(h):
            for x in range(w):
                if x == 2 and y == 2:
                    continue
                pos = (x, y, d)
                bug = cells[pos]
                ns = neighbors_folded(w, h, cells, pos)
                n = nb_alive(ns)
                if bug and n == 1:
                    print('xxxxx')
                    result[pos] = True
                if (not bug) and (n == 1 or n == 2):
                    print('xxxxx')
                    result[pos] = True
    return result


def neighbors_folded(w, h, cells, pos):
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
    elif x == w-1:
        return [cells[(3, 2, d-1)]]
    elif y == 0:
        return [cells[(2, 2, d-1)]]
    elif y == h-1:
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
    w, h, cells = parse(lines)
    print(solve_1(w, h, cells.copy()))
    print(solve_2(w, h, cells))
