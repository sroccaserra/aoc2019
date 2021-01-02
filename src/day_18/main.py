import fileinput
import sys


def parse(lines):
    for line in lines:
        print(line)


if __name__ == '__main__' and not sys.flags.interactive:
    lines = [l.strip() for l in fileinput.input()]
    parse(lines)
