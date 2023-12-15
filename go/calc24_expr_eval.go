package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"time"
)

type OpType int

const (
	None OpType = iota
	Add
	Substract
	Multiply
	Divide
)

type Operand struct {
	index int
	op    OpType
	left  *Operand
	right *Operand
}

func (x *Operand) eval(nums []int) float64 {
	switch x.op {
	case None:
		return float64(nums[x.index])
	case Add:
		return x.left.eval(nums) + x.right.eval(nums)
	case Substract:
		return x.left.eval(nums) - x.right.eval(nums)
	case Multiply:
		return x.left.eval(nums) * x.right.eval(nums)
	case Divide:
		return x.left.eval(nums) / x.right.eval(nums)
	}
	return 0
}

func (x *Operand) toStringHelper(nums []int, is_denominator bool) string {
	if x.op == Add || x.op == Substract || (is_denominator && x.op != None) {
		return fmt.Sprintf("(%s)", x.toString(nums))
	}
	return x.toString(nums)
}

func (x *Operand) toString(nums []int) string {
	switch x.op {
	case None:
		return strconv.Itoa(nums[x.index])
	case Add:
		return fmt.Sprintf("%s + %s", x.left.toString(nums), x.right.toString(nums))
	case Substract:
		return fmt.Sprintf("%s - %s", x.left.toString(nums), x.right.toStringHelper(nums, false))
	case Multiply:
		return fmt.Sprintf("%s * %s", x.left.toStringHelper(nums, false), x.right.toStringHelper(nums, false))
	case Divide:
		return fmt.Sprintf("%s / %s", x.left.toStringHelper(nums, false), x.right.toStringHelper(nums, true))
	}
	return ""
}

type Calc24 struct {
	expressions []*Operand
}

func generate(indexes []*Operand) []*Operand {
	n := len(indexes)
	if n == 1 {
		return indexes
	} else {
		var result []*Operand
		for i := 0; i < n; i++ {
			for j := i + 1; j < n; j++ {
				var reduced []*Operand
				for k := 0; k < n; k++ {
					if k == i || k == j {
						continue
					}
					reduced = append(reduced, indexes[k])
				}
				reduced = append(reduced, nil)
				for _, x := range combine(indexes[i], indexes[j]) {
					reduced[n-2] = x
					r := generate(reduced)
					result = append(result, r...)
				}
			}
		}
		return result
	}
}

func NewCalc24(n int) Calc24 {
	var indexes []*Operand
	for j := 0; j < n; j++ {
		op := new(Operand)
		op.index = j
		indexes = append(indexes, op)
	}
	return Calc24{expressions: generate(indexes)}
}

func (c *Calc24) Calc(nums []int) (string, bool) {
	target := float64(24)
	for _, expr := range c.expressions {
		if expr.eval(nums) == target {
			return expr.toString(nums), true
		}
	}
	return "", false
}

func combine(num1, num2 *Operand) []*Operand {
	result := []*Operand{
		{index: -1, op: Add, left: num1, right: num2},
		{index: -1, op: Multiply, left: num1, right: num2},
		{index: -1, op: Substract, left: num1, right: num2},
		{index: -1, op: Substract, left: num2, right: num1},
		{index: -1, op: Divide, left: num1, right: num2},
		{index: -1, op: Divide, left: num2, right: num1},
	}
	return result
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	size := 4
	calc24 := NewCalc24(size)
	for i := 0; i < 1000; i++ {
		var nums []int
		for j := 0; j < size; j++ {
			nums = append(nums, rand.Intn(13)+1)
		}
		result, success := calc24.Calc(nums)
		if success {
			fmt.Printf("%v -> %s\n", nums, result)
		} else {
			fmt.Printf("%v -> No Solution\n", nums)
		}
	}
}
