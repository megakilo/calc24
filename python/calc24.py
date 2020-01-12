#! /usr/bin/env python3

import random
import sys
import collections

Number = collections.namedtuple("Number", "value expr op")


def create_expr(num, is_denominator):
    if num.op in ('+', '-') or (is_denominator and num.op == '*'):
        return f"({num.expr})"
    else:
        return num.expr


def combine(num1, num2):
    result = []
    result.append(Number(num1.value + num2.value,
                         f"{num1.expr} + {num2.expr}", '+'))
    result.append(Number(num1.value * num2.value,
                         f"{create_expr(num1, False)} * {create_expr(num2, False)}", '*'))
    if num1.value > num2.value:
        result.append(Number(num1.value - num2.value,
                             f"{num1.expr} - {create_expr(num2, False)}", '-'))
    else:
        result.append(Number(num2.value - num1.value,
                             f"{num2.expr} - {create_expr(num1, False)}", '-'))
    if not num2.value == 0:
        result.append(Number(num1.value / num2.value,
                             f"{create_expr(num1, False)} / {create_expr(num2, True)}", '/'))
    if not num1.value == 0:
        result.append(Number(num2.value / num1.value,
                             f"{create_expr(num2, False)} / {create_expr(num1, True)}", '/'))
    return result


def calc(nums, target):
    if len(nums) == 1:
        if nums[0].value == target:
            return nums[0].expr
        return None
    for i in range(len(nums)):
        for j in range(i+1, len(nums)):
            reduced = []
            for k in range(len(nums)):
                if k != i and k != j:
                    reduced.append(nums[k])
            for x in combine(nums[i], nums[j]):
                reduced.append(x)
                result = calc(reduced, target)
                if result is not None:
                    return result
                reduced = reduced[:len(reduced)-1]
    return None


if __name__ == '__main__':
    for c in range(1000):
        vector = [random.randint(1, 13) for x in range(4)]
        numbers = [Number(float(x), str(x), 'x') for x in vector]
        result = calc(numbers, 24)
        if result is None:
            print(f"{vector} -> No Solution")
        else:
            print(f"{vector} -> {result}")
