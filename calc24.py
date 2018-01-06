#! /usr/bin/env python3

import itertools
import random
import sys

MIN_PYTHON = (3, 0)
if sys.version_info < MIN_PYTHON:
    sys.exit("Python %s.%s or later is required.\n" % MIN_PYTHON)

def compute(numbers, target):
    assert numbers
    if len(numbers) == 1:
        if target == numbers[0]["value"]:
            print("Solution: " + numbers[0]["expr"])
            return True
        else:
            return False
    pick_two = itertools.permutations(range(len(numbers)), 2)
    for index in pick_two:
        rest = [numbers[i] for i in range(len(numbers)) if i not in index]
        idx1, idx2 = index
        for infix in ["+", "-", "*", "/"]:
            if infix == "/" and numbers[idx2]["value"] == 0:
                continue
            expr = "(%s%s%s)" % (numbers[idx1]["expr"], infix, numbers[idx2]["expr"])
            value = eval("(%s%s%s)" % (numbers[idx1]["value"], infix, numbers[idx2]["value"]))
            if value < 0 or value < 1:
                continue
            if compute(rest+[{"value":value, "expr":expr}], target) is True:
                return True
    return False

if __name__ == '__main__':
    random_numbers = [random.randint(1, 13) for x in range(4)]
    print(random_numbers)
    print("="*len(str(random_numbers)))
    processed_numbers = [{"value":x, "expr":str(x)} for x in random_numbers]
    if compute(processed_numbers, 24) is False:
        print("No Solution")
