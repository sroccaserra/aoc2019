import fileinput
import sys

NB_CARDS = 10007
# NB_CARDS = 10


def main():
    lines = [line.strip() for line in fileinput.input()]
    shuffles = parse(lines)
    deck = list(range(NB_CARDS))
    result = solve_1(deck, shuffles)
    print(result)


def solve_1(deck, shuffles):
    for shuffle in shuffles:
        deck = shuffle(deck)
    return deck.index(2019)
    # return deck


def parse(lines):
    return [parse_line(line) for line in lines]


def parse_line(line):
    ws = line.split()
    if ws[0] == 'cut':
        return lambda deck: cut(int(ws[1]), deck)
    elif ws[0] == 'deal':
        if ws[1] == 'into':
            return new_stack
        elif ws[1] == 'with':
            return lambda deck: increment(int(ws[3]), deck)

    raise 'impossible'


def cut(n, deck):
    return deck[n:] + deck[:n]


def new_stack(deck):
    deck.reverse()
    return deck


def increment(n, deck):
    result = [-1] * NB_CARDS
    for i in range(NB_CARDS):
        result[(i*n) % NB_CARDS] = deck[i]
    return result


if __name__ == '__main__' and not sys.flags.interactive:
    main()
