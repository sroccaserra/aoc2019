import fileinput
import sys

NB_CARDS = 10007
# NB_CARDS = 10


def main():
    lines = [line.strip() for line in fileinput.input()]
    shuffles = parse(lines)
    print(solve_1(2019, shuffles))


def solve_1(i, shuffles):
    for shuffle in shuffles:
        i = shuffle(i)
    return i


def parse(lines):
    return [parse_line(line) for line in lines]


def parse_line(line):
    ws = line.split()
    if ws[0] == 'cut':
        return lambda i: cut(int(ws[1]), i)
    elif ws[0] == 'deal':
        if ws[1] == 'into':
            return new_stack
        elif ws[1] == 'with':
            return lambda i: increment(int(ws[3]), i)

    raise 'impossible'


def cut(n, i):
    return (i - n) % NB_CARDS


def new_stack(i):
    return NB_CARDS - 1 - i


def increment(n, i):
    return (n * i) % NB_CARDS




if __name__ == '__main__' and not sys.flags.interactive:
    main()
