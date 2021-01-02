import fileinput
import collections
import sys


def solve(lines):
    start = find_start(lines)
    visited = {}
    found_keys = ''
    return find_minimum_distance(lines, start, visited, found_keys)


def find_minimum_distance(lines, start, visited, found_keys):
    sorted_keys = ''.join(sorted(found_keys))

    if (start, sorted_keys) in visited:
        return visited[start, sorted_keys]

    if len(visited) % 10 == 0:
        print(sorted_keys)

    keys = reachable_key_distances(lines, start, found_keys)
    if 0 == len(keys):
        result = 0
    else:
        poss = []
        for c, (distance, point) in keys.items():
            d = find_minimum_distance(lines, point, visited, found_keys+c)
            poss.append(distance + d)
        result = min(poss)
    visited[start, sorted_keys] = result
    return result


def reachable_key_distances(lines, start, found_keys):
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

            if is_door(c) and c.lower() not in found_keys:
                continue

            if is_key(c) and c not in found_keys:
                key_distances[c] = distances[neighbor], neighbor
            else:
                to_explore.append(neighbor)

    return key_distances


def find_start(lines):
    for y in range(len(lines)):
        for x in range(len(lines[0])):
            if char_at(lines, (x, y)) == '@':
                return x, y


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
    w = len(lines[0])
    h = len(lines)
    for y in range(0, h):
        for x in range(0, w):
            c = char_at(lines, (x, y))
            print(c, end='')
        print('')


if __name__ == '__main__' and not sys.flags.interactive:
    lines = [line.strip() for line in fileinput.input()]
    print(solve(lines))
