import fileinput
import sys


def solve(lines):
    w = len(lines[0])
    h = len(lines)
    for j in range(h-99):
        for i in range(w-99):
            if lines[j][i] == '1' and lines[j][i+99] == '1':
                if lines[j+99][i] == '1':
                    return i + 10000*j


if __name__ == "__main__" and not sys.flags.interactive:
    lines = [line.strip() for line in fileinput.input()]
    print(solve(lines))
