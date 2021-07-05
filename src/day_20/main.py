import fileinput
from queue import Queue

UNSEEN = -1
A = ord('A')
Z = ord('Z')


def solve_1(lines):
    m = parse_labyrinth(lines)
    portals = find_portals(m)
    AA = portals['AA'][0]
    ZZ = portals['ZZ'][0]
    wormholes = dict()
    print(portals)
    for [a, b] in [vv for vv in portals.values() if len(vv) == 2]:
        wormholes[a] = b
        wormholes[b] = a
    mark(m, wormholes, ZZ, AA)
    show(m, lines)
    return m[ZZ]


def show(m, lines):
    for y in range(H):
        print()
        for x in range(W):
            n = m.get((x, y))
            c = lines[y][x]
            if type(n) is int:
                print(str(n).rjust(3), end='')
            elif c == '#' or is_letter(c):
                print(c.center(3, ':'), end='')
            else:
                print('   ', end='')
    print()


def mark(m, wormholes, ZZ, AA):
    todo = Queue()
    todo.put(AA)
    m[AA] = 0
    while m[ZZ] is UNSEEN:
        p = todo.get()
        n = m.get(p)

        for neighbor in neighbors(m, wormholes, p[0], p[1]):
            if m[neighbor] is not UNSEEN:
                continue

            m[neighbor] = n+1
            todo.put(neighbor)


def parse_labyrinth(lines):
    m = dict()
    for y in range(H):
        line = lines[y]
        for x in range(W):
            c = line[x]
            if is_letter(c):
                m[(x, y)] = c
            if '.' == c:
                m[(x, y)] = UNSEEN
    return m


def is_letter(c):
    if type(c) is not str:
        return False
    n = ord(c)
    return A <= n and n <= Z


def find_portals(m):
    portals = dict()
    for y in range(H):
        for x in range(W):
            c = m.get((x, y))
            if is_letter(c):
                c_r = m.get((x+1, y))
                if is_letter(c_r):
                    for p in [(x+2, y), (x-1, y)]:
                        if UNSEEN == m.get(p):
                            key = c+c_r
                            values = portals.get(key, [])
                            values.append(p)
                            portals[key] = values
                c_b = m.get((x, y+1))
                if is_letter(c_b):
                    for p in [(x, y+2), (x, y-1)]:
                        if UNSEEN == m.get(p):
                            key = c+c_b
                            values = portals.get(key, [])
                            values.append(p)
                            portals[key] = values
    return portals


def neighbors(m, wormholes, x, y):
    result = []
    for p in [(x, y-1), (x-1,  y), (x+1,  y), (x,  y+1)]:
        if type(m.get(p)) is int:
            result.append((p))
    wh = wormholes.get((x, y))
    if wh is not None:
        result.append(wh)
    return result


lines = [line for line in fileinput.input()]
W = len(lines[0])
H = len(lines)
print(solve_1(lines))
