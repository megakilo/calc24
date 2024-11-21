#! /usr/bin/env python3

import sys

import ast
import operator as op
from fractions import Fraction

# supported operators
operators = {ast.Add: op.add, ast.Sub: op.sub, ast.Mult: op.mul, ast.Div: op.truediv}

def eval_expr(expr):
    values = []

    def _eval(node):
        nonlocal values
        if isinstance(node, ast.Constant) and isinstance(node.value, int):
            values.append(node.value)
            return Fraction(node.value)
        elif isinstance(node, ast.BinOp):
            left_value = _eval(node.left)
            right_value = _eval(node.right)
            return operators[type(node.op)](left_value, right_value)
        elif isinstance(node, ast.UnaryOp):
            operand_value = _eval(node.operand)
            return operators[type(node.op)](operand_value)
        else:
            raise TypeError(node)

    return (_eval(ast.parse(expr, mode='eval').body), values)
        
total = 0
correct = 0
has_result = 0
no_result = 0
invalid = 0
bad_results = []

target = Fraction(24)

for line in sys.stdin:
    line = line.strip()
    print(line)
    parts = line.split("->")
    if len(parts) != 2:
        invalid += 1
    else:
        total += 1
        try:
            result, values = eval_expr(parts[1].strip())
            has_result += 1
            inputs = [int(i.strip()) for i in parts[0].split(",")]
            if result == target and sorted(values) == sorted(inputs):
                correct += 1
            else:
                bad_results.append(line)
        except:
            no_result += 1
            pass

print("=============")
print(f"Total Lines: {total}")
print(f"Invalid Format: {invalid}")
print(f"No Result Count: {no_result}")
if total > 0:
    print(f"No Result Ratio: {(100 * no_result / total):.2f}%")
print(f"Has Result Count: {has_result}")
print(f"Correct Result Count: {correct}")
if has_result > 0:
    print(f"Correct Result Ratio: {(100 * correct / has_result):.2f}%")
if len(bad_results) > 0:
    print("Bad Results: ", bad_results)