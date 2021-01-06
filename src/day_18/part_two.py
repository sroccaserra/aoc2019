# See: https://github.com/sophiebits/adventofcode/blob/master/2019/day18.py

import fileinput
import collections
import sys


def solve(maze):
    starts = find_start(maze)
    visited = {}
    found_keys = ''
    return find_minimum_distance(maze, starts, visited, found_keys)


def find_minimum_distance(maze, starts, visited, found_keys):
    sorted_keys = ''.join(sorted(found_keys))

    if (starts, sorted_keys) in visited:
        return visited[starts, sorted_keys]

    if len(visited) % 10 == 0:
        print(sorted_keys)

    keys = reachable_keys_n(maze, starts, found_keys)
    if 0 == len(keys):
        result = 0
    else:
        choices = []
        for c, (distance, key_xy, key_index) in keys.items():
            new_starts = replace_start_with_key(key_xy, key_index, starts)
            d = find_minimum_distance(maze, new_starts, visited, found_keys+c)
            choices.append(distance + d)
        result = min(choices)
    visited[starts, sorted_keys] = result
    return result


def replace_start_with_key(key_xy, key_i, starts):
    xs = [key_xy if key_i == i else xy for i, xy in enumerate(starts)]
    return tuple(xs)


def reachable_keys_n(maze, starts, found_keys):
    keys = {}
    for i, start in enumerate(starts):
        reachable_i = reachable_key_distances(maze, start, found_keys)
        for c, (d, xy) in reachable_i.items():
            keys[c] = d, xy, i
    return keys


def reachable_key_distances(maze, start, found_keys):
    distances = {start: 0}
    key_distances = {}
    to_explore = collections.deque([start])
    while to_explore:
        xy = to_explore.popleft()
        for neighbor in neighbors(xy):
            c = char_at(maze, neighbor)

            if not is_in_bounds(maze, neighbor):
                continue
            if '#' == c:
                continue
            if neighbor in distances:
                continue

            distances[neighbor] = distances[xy] + 1

            if is_door(c) and c.lower() not in found_keys:
                continue

            if is_key(c) and c not in found_keys:
                key_distances[c] = distances[neighbor], neighbor
            else:
                to_explore.append(neighbor)

    return key_distances


def find_start(maze):
    starts = []
    for y in range(len(maze)):
        for x in range(len(maze[0])):
            if char_at(maze, (x, y)) == '@':
                starts.append((x, y))
    return tuple(starts)


def char_at(maze, xy):
    x, y = xy
    return maze[y][x]


def is_in_bounds(maze, xy):
    x, y = xy
    return (0 <= x < len(maze[0])) and (0 <= y < len(maze))


def neighbors(xy):
    x, y = xy
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
