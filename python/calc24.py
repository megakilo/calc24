#! /usr/bin/env python3

from fractions import Fraction
import random


class Number:
    def __init__(self, num1, num2, op):
        self.left = num1
        self.right = num2
        self.op = op
        if op == '+':
            self.value = num1.value + num2.value
        elif op == '-':
            self.value = num1.value - num2.value
        elif op == '*':
            self.value = num1.value * num2.value
        elif op == '/':
            self.value = num1.value / num2.value

    @staticmethod
    def Create(value):
        x = Number(None, None, 'x')
        x.value = Fraction(value)
        return x

    @staticmethod
    def AddParentheses(node, is_denominator):
        if node.op in ('+', '-') or (is_denominator and node.op != 'x'):
            return f"({node})"
        else:
            return node

    def __str__(self):
        if self.op == 'x':
            return str(self.value)
        if self.op == '+':
            return f"{self.left} + {self.right}"
        elif self.op == '-':
            return f"{self.left} - {Number.AddParentheses(self.right, False)}"
        elif self.op == '*':
            return f"{Number.AddParentheses(self.left, False)} * {Number.AddParentheses(self.right, False)}"
        elif self.op == '/':
            return f"{Number.AddParentheses(self.left, False)} / {Number.AddParentheses(self.right, True)}"

    def __repr__(self):
        return self.__str__()


def combine(num1, num2):
    yield Number(num1, num2, '+')
    yield Number(num1, num2, '*')
    if num1.value > num2.value:
        yield Number(num1, num2, '-')
    else:
        yield Number(num2, num1, '-')
    if num2.value != 0:
        yield Number(num1, num2, '/')
    if num1.value != 0:
        yield Number(num2, num1, '/')


def calc(nodes, target):
    if len(nodes) == 1:
        if nodes[0].value == target:
            return nodes[0]
        return None
    for i in range(len(nodes)):
        for j in range(i+1, len(nodes)):
            reduced = []
            for k in range(len(nodes)):
                if k != i and k != j:
                    reduced.append(nodes[k])
            for x in combine(nodes[i], nodes[j]):
                reduced.append(x)
                result = calc(reduced, target)
                if result:
                    return result
                reduced.pop()
    return None

def calc24(numbers):
    nodes = [Number.Create(x) for x in numbers]
    result = calc(nodes, 24)
    if result:
        return f"{numbers} -> {result}"
    else:
        return f"{numbers} -> No Solution"

if __name__ == '__main__':
    for c in range(1000):
        numbers = [random.randint(1, 13) for x in range(4)]
        print(calc24(numbers))
