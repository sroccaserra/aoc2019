import fileinput
import sys
from functools import reduce

NB_CARDS = 10007
# NB_CARDS = 10


def main():
    lines = [line.strip() for line in fileinput.input()]
    shuffles = parse(lines)
    print(solve_1(2019, shuffles))


def solve_1(i, shuffles):
    shuffle = reduce(combine, shuffles, (1, 0))
    return (shuffle[0]*i + shuffle[1]) % NB_CARDS


def parse(lines):
    return [parse_line(line) for line in lines]


def parse_line(line):
    ws = line.split()
    if ws[0] == 'cut':
        return cut(int(ws[1]))
    elif ws[0] == 'deal':
        if ws[1] == 'into':
            return new_stack()
        elif ws[1] == 'with':
            return increment(int(ws[3]))

    raise 'impossible'


# c(ax + b) + d -> cax + cb + d
def combine(f, g):
    return (f[0]*g[0], f[1]*g[0] + g[1])


def cut(n):
    return (1, -n)


def new_stack():
    return (-1, -1)


def increment(n):
    return (n, 0)


if __name__ == '__main__' and not sys.flags.interactive:
    main()
