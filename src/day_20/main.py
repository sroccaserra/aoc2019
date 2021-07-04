import fileinput

UNSEEN = -1
A = ord('A')
Z = ord('Z')


def solve(lines):
    labyrinth = create_labyrinth(lines)
    portals = find_portals(labyrinth)
    return [p for p in portals if len(portals[p]) != 2]


def create_labyrinth(lines):
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


def neighbors(m, x, y):
    return [m.get((x+i, y+j)) for i, j in [
        (-1, -1), (0, -1), (1, -1),
        (-1,  0),          (1,  0),
        (-1,  1), (0,  1), (1,  1)]]


lines = [line for line in fileinput.input()]
W = len(lines[0])
H = len(lines)
print(solve(lines))
