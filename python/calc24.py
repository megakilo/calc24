#! /usr/bin/env python3

from enum import Enum
from dataclasses import dataclass
import random


class OpType(Enum):
    NONE = 0
    ADD = 1
    SUBSTRACT = 2
    MULTIPLY = 3
    DIVIDE = 4


@dataclass
class Operand:
    index: int
    op: OpType = OpType.NONE
    left: 'Operand' = None
    right: 'Operand' = None

    def to_string_helper(self, nums, is_denominator=False):
        if self.op == OpType.ADD or self.op == OpType.SUBSTRACT or (is_denominator and self.op != OpType.NONE):
            return f"({self.to_string(nums)})"
        else:
            return self.to_string(nums)

    def to_string(self, nums):
        if self.op == OpType.NONE:
            return str(nums[self.index])
        if self.op == OpType.ADD:
            return f"{self.left.to_string(nums)} + {self.right.to_string(nums)}"
        elif self.op == OpType.SUBSTRACT:
            return f"{self.left.to_string(nums)} - {self.right.to_string_helper(nums)}"
        elif self.op == OpType.MULTIPLY:
            return f"{self.left.to_string_helper(nums)} * {self.right.to_string_helper(nums)}"
        elif self.op == OpType.DIVIDE:
            return f"{self.left.to_string_helper(nums)} / {self.right.to_string_helper(nums, True)}"

    def eval(self, nums):
        if self.op == OpType.NONE:
            return float(nums[self.index])
        if self.op == OpType.ADD:
            return self.left.eval(nums) + self.right.eval(nums)
        elif self.op == OpType.SUBSTRACT:
            return self.left.eval(nums) - self.right.eval(nums)
        elif self.op == OpType.MULTIPLY:
            return self.left.eval(nums) * self.right.eval(nums)
        elif self.op == OpType.DIVIDE:
            return self.left.eval(nums) / self.right.eval(nums)


def combine(op1, op2):
    return [
        Operand(-1, OpType.ADD, op1, op2),
        Operand(-1, OpType.MULTIPLY, op1, op2),
        Operand(-1, OpType.SUBSTRACT, op1, op2),
        Operand(-1, OpType.SUBSTRACT, op2, op1),
        Operand(-1, OpType.DIVIDE, op1, op2),
        Operand(-1, OpType.DIVIDE, op2, op1),
    ]


def generate_formula(indexes):
    n = len(indexes)
    if n == 1:
        return indexes
    result = []
    for i in range(n):
        for j in range(i+1, n):
            reduced = [x for k, x in enumerate(indexes) if k != i and k != j]
            for r in combine(indexes[i], indexes[j]):
                reduced.append(r)
                result.extend(generate_formula(reduced))
                reduced.pop()
    return result


class Calc24:
    def __init__(self, n) -> None:
        indexes = [Operand(i) for i in range(n)]
        self.__formula = generate_formula(indexes)

    def calc(self, nums) -> str:
        target = float(24)
        for f in self.__formula:
            try:
                if f.eval(nums) == target:
                    return f.to_string(nums)
            except:
                continue
        return "No Solution"


if __name__ == '__main__':
    size = 4
    calc24 = Calc24(size)
    for _ in range(1000):
        nums = [random.randint(1, 13) for x in range(size)]
        result = calc24.calc(nums)
        print(f"{nums} -> {result}")
