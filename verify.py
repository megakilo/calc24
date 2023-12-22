#! /usr/bin/env python3

import sys

total = 0
correct = 0
invalid_lines = []

for line in sys.stdin:
    print(line, end = "")
    parts = line.split("->")
    if len(parts) != 2:
        invalid_lines.append(line)
    else:
        
        try:
            result = eval(parts[1])
            total += 1
            if result == 24:
                 correct += 1
        except:
            pass
print("=============")
print(f"Passed result: {correct}/{total}")