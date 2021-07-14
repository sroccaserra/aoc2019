import fileinput
import sys
from functools import reduce

# NB_CARDS = 10007
# NB_CARDS = 10
NB_CARDS = 119315717514047
NB_PROCESSES = 101741582076661


def main():
    lines = [line.strip() for line in fileinput.input()]
    shuffles = parse(lines)
    # print(solve_1(2019, shuffles))
    print(solve_2(2020, shuffles))


def solve_1(i, shuffles):
    shuffle = reduce(combine, shuffles, (1, 0))
    return (shuffle[0]*i + shuffle[1]) % NB_CARDS


# Appliquer n fois une fonction affine :
# ax + b
# a(ax+b) + b = a^2x + ab + b
# a(a^2x + ab + b) = a^3x + a^2b + ab + b
# ...
# La pente est a^n, et l'ordonnée à l'origine est la somme de la suite
# géométrique de raison a et de premier terme b
# a^n x + b * (a^n - 1) / (a - 1)
def solve_2(i, shuffles):
    shuffle = invert(reduce(combine, shuffles, (1, 0)))
    a = pow(shuffle[0], NB_PROCESSES, NB_CARDS)
    b = shuffle[1]*(a + NB_CARDS - 1)*mul_inv(shuffle[0]-1, NB_CARDS)
    return (a*i + b) % NB_CARDS


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
    return ((f[0]*g[0]) % NB_CARDS, (f[1]*g[0] + g[1]) % NB_CARDS)


def cut(n):
    return (1, -n)


def new_stack():
    return (-1, -1)


def increment(n):
    return (n, 0)


# trouver c et d tels que
# c(ax + b) + d -> cax + cb + d -> x
def invert(f):
    c = mul_inv(f[0], NB_CARDS)
    d = (-c*f[1]) % NB_CARDS
    return (c, d)


def mul_inv(a, b):
    """
    - https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
    """
    b0 = b
    x0, x1 = 0, 1
    if b == 1:
        return 1
    while a > 1:
        q = a // b
        a, b = b, a % b
        x0, x1 = x1 - q * x0, x0
    if x1 < 0:
        x1 += b0
    return x1


if __name__ == '__main__' and not sys.flags.interactive:
    main()
