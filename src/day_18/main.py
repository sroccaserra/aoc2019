import fileinput
import collections
import sys


def solve(lines):
    start = find_start(lines)
    return reachable_key_distances(lines, start)


def find_start(lines):
    for y in range(len(lines)):
        for x in range(len(lines[0])):
            if char_at(lines, (x, y)) == '@':
                return (x, y)


def reachable_key_distances(lines, start):
    distances = {start: 0}
    key_distances = {}
    to_explore = collections.deque([start])
    while to_explore:
        point = to_explore.popleft()
        for neighbor in neighbors(point):
            c = char_at(lines, neighbor)

            if not is_in_bounds(lines, neighbor):
                continue
            if '#' == c:
                continue
            if neighbor in distances:
                continue

            distances[neighbor] = distances[point] + 1

            if is_door(c):
                continue

            if is_key(c):
                key_distances[c] = distances[neighbor], neighbor
            else:
                to_explore.append(neighbor)

    return distances


def char_at(lines, point):
    x, y = point
    return lines[y][x]


def is_in_bounds(lines, point):
    x, y = point
    return (0 <= x < len(lines[0])) and (0 <= y < len(lines))


def neighbors(point):
    x, y = point
    return [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]


def is_key(c):
    return 'a' <= c <= 'z'


def is_door(c):
    return 'A' <= c <= 'Z'


def display_maze(lines, distances):
    for y in range(0, len(lines)):
        for x in range(0, len(lines[0])):
            p = (x, y)
            if p in distances:
                d = distances[p]
                print(d % 10, end='')
            else:
                c = char_at(lines, p)
                print(c, end='')
        print('')


if __name__ == '__main__' and not sys.flags.interactive:
    lines = [line.strip() for line in fileinput.input()]
    distances = solve(lines)
    display_maze(lines, distances)
