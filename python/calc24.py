#! /usr/bin/env python3

import random
import sys

MIN_PYTHON = (3, 0)
if sys.version_info < MIN_PYTHON:
    sys.exit("Python %s.%s or later is required.\n" % MIN_PYTHON)

def combine2(nums):
    result = []
    result.append((nums[0][0] + nums[1][0], "({} + {})".format(nums[0][1], nums[1][1])))
    result.append((nums[0][0] * nums[1][0], "({} * {})".format(nums[0][1], nums[1][1])))
    if nums[0][0] > nums[1][0]:
        result.append((nums[0][0] - nums[1][0], "({} - {})".format(nums[0][1], nums[1][1])))
    else:
        result.append((nums[1][0] - nums[0][0], "({} - {})".format(nums[1][1], nums[0][1])))
    if nums[1][0] != 0:
        result.append((nums[0][0] / nums[1][0], "({} / {})".format(nums[0][1], nums[1][1])))
    if nums[0][0] != 0:
        result.append((nums[1][0] / nums[0][0], "({} / {})".format(nums[1][1], nums[0][1])))
    return result

def split(nums, n):
    if n == 0:
        return [([], nums)]
    if len(nums) <= n:
        return [(nums, [])]
    head = nums[0]
    tail = nums[1:]
    return [(v[0], v[1] + [head]) for v in split(tail, n)] + [(v[0] + [head], v[1]) for v in split(tail, n-1)]

def reduce(nums):
    result = []
    for pair in split(nums, 2):
        for r in combine2(pair[0]):
            result.append(pair[1] + [r])
    return result

def calc(nums):
    if len(nums) == 1:
        return nums
    result = []
    for xs in reduce(nums):
        result.extend(calc(xs))
    return result

if __name__ == '__main__':
    for c in range(1000):
        vector = [random.randint(1, 13) for x in range(4)]
        numbers = [(float(x), str(x)) for x in vector]
        solution = next((x[1] for x in calc(numbers) if x[0] == 24), "No Solution")
        print("{} -> {}".format(vector, solution))
