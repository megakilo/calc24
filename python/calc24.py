#! /usr/bin/env python

import random
import sys
import collections

Number = collections.namedtuple("Number", "value expr op")

def addParentheses(num, conditions):
    if num.op in conditions:
        return "({})".format(num.expr)
    else:
        return num.expr

def combine(num1, num2):
    result = []
    result.append(Number(num1.value + num2.value, "{} + {}".format(num1.expr, num2.expr), '+'))
    result.append(Number(num1.value * num2.value, "{} * {}".format(
        addParentheses(num1, "+-"), addParentheses(num2, "+-")), '*'))
    if num1.value > num2.value:
        result.append(Number(num1.value - num2.value, "{} - {}".format(num1.expr, addParentheses(num2, "+-")), '-'))
    else:
        result.append(Number(num2.value - num1.value, "{} - {}".format(num2.expr, addParentheses(num1, "+-")), '-'))
    if not num2.value == 0:
        result.append(Number(num1.value / num2.value, "{} / {}".format(
            addParentheses(num1, "+-"), addParentheses(num2, "+-*")), '/'))
    if not num1.value == 0:
        result.append(Number(num2.value / num1.value, "{} / {}".format(
            addParentheses(num2, "+-"), addParentheses(num1, "+-*")), '/'))
    return result

def calc(nums, target):
    if len(nums) == 1:
        if nums[0].value == target:
            return nums[0].expr
        return None
    for i in range(len(nums)):
        for j in range(i+1, len(nums)):
            reduced = nums[:]
            reduced[i] = reduced[len(reduced)-1]
            reduced[j] = reduced[len(reduced)-2]
            reduced = reduced[:len(reduced)-2]
            for x in combine(nums[i], nums[j]):
                reduced.append(x)
                result = calc(reduced, target)
                if not result is None:
                    return result
                reduced = reduced[:len(reduced)-1]
    return None

if __name__ == '__main__':
    for c in range(1000):
        vector = [random.randint(1, 13) for x in range(4)]
        numbers = [Number(float(x), str(x), 'x') for x in vector]
        result = calc(numbers, 24)
        if result is None:
            print("{} -> No Solution".format(vector))
        else:
            print("{} -> {}".format(vector, result))
