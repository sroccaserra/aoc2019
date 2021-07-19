import fileinput
import sys

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

def show(cells, depth):
    levels = []
    for d in range(-depth, depth+1):
        lines = []
        for y in range(H):
            line = []
            for x in range(W):
                bug = (x, y, d) in cells
                c = '#' if bug else '.'
                if c == '.' and (x, y) == (2, 2):
                    line.append('?')
                else:
                    line.append(c)
            lines.append(''.join(line))
        levels.append('{}\n'.format(d)+'\n'.join(lines))
    return '\n\n'.join(levels)


def solve_2(initial_cells):
    cells = set()
    for (x, y) in initial_cells:
        cells.add((x, y, 0))
    # print(show(cells, 0))
    # print('\n-----\n')
    minutes = 200
    for t in range(1, minutes + 1):
        cells = step_folded(t, cells)
    # print(show(cells, minutes//2))
    return len(cells)


debug = None  # (0, 4, 1)


def step_folded(depth, cells):
    result = set()
    for d in range(-depth - 1, depth+2):
        for y in range(H):
            for x in range(W):
                if x == 2 and y == 2:
                    continue
                pos = (x, y, d)
                bug = pos in cells
                ns = neighbors_folded(cells, pos)
                n = nb_alive(ns)
                if bug and n == 1:
                    result.add(pos)
                if (not bug) and (n == 1 or n == 2):
                    result.add(pos)
                global debug
                if (x, y, d) == debug:
                    print(debug, debug in result, ns)
    return result


def neighbors_folded(cells, pos):
    x, y, d = pos

    result = []
    for dx, dy in [(0, -1), (1, 0), (0, 1), (-1, 0)]:
        result.append((x+dx, y+dy, d) in cells)

    return result + \
        neighbors_folded_outer(cells, pos) + \
        neighbors_folded_inner(cells, pos)


def neighbors_folded_outer(cells, pos):
    x, y, d = pos
    result = []

    if x == 0:
        result.append((1, 2, d-1) in cells)
    elif x == W-1:
        result.append((3, 2, d-1) in cells)

    if y == 0:
        result.append((2, 1, d-1) in cells)
    elif y == H-1:
        result.append((2, 3, d-1) in cells)
    return result


def neighbors_folded_inner(cells, pos):
    x, y, d = pos

    if x == 2 and y == 1:
        return [(i, 0, d+1) in cells for i in range(W)]

    if x == 2 and y == 3:
        return [(i, H-1, d+1) in cells for i in range(W)]

    if y == 2 and x == 1:
        return [(0, j, d+1) in cells for j in range(H)]

    if y == 2 and x == 3:
        return [(W-1, j, d+1) in cells for j in range(H)]

    return []


if __name__ == '__main__' and not sys.flags.interactive:
    lines = [line.strip() for line in fileinput.input()]
    cells = parse(lines)
    print(solve_1(cells.copy()))
    print(solve_2(cells))
