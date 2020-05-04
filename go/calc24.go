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
	Plus
	Minus
	Multiply
	Divide
)

type Number struct {
	value float32
	op    OpType
	left  *Number
	right *Number
}

func AddParentheses(x *Number, is_denominator bool) string {
	if x.op == Plus || x.op == Minus || (is_denominator && x.op != None) {
			return fmt.Sprintf("(%s)", Print(x))
	}
	return Print(x)
}

func Print(x *Number) string {
	switch x.op {
	case None:
		return strconv.Itoa(int(x.value))
	case Plus:
		return fmt.Sprintf("%s + %s", Print(x.left), Print(x.right))
	case Minus:
		return fmt.Sprintf("%s - %s", Print(x.left), AddParentheses(x.right, false))
	case Multiply:
		return fmt.Sprintf("%s * %s", AddParentheses(x.left, false), AddParentheses(x.right, false))
	case Divide:
		return fmt.Sprintf("%s / %s", AddParentheses(x.left, false), AddParentheses(x.right, true))
	}
	return "";
}

func CreateNumber(num1, num2 *Number, op OpType) *Number {
	x := new(Number)
	x.op = op
	x.left = num1
	x.right = num2
	switch op {
	case Plus:
		x.value = num1.value + num2.value
	case Minus:
		x.value = num1.value - num2.value
	case Multiply:
		x.value = num1.value * num2.value
	case Divide:
		x.value = num1.value / num2.value
	}
	return x
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	for i := 0; i < 1000; i++ {
		var vector []int
		for j := 0; j < 4; j++ {
			vector = append(vector, rand.Intn(13)+1)
		}
		var nums []*Number
		for _, v := range vector {
			nums = append(nums, &Number{float32(v), None, nil, nil})
		}
		result, found := calc(nums, 24)
		if found {
			fmt.Printf("%v -> %s\n", vector, Print(result))
		} else {
			fmt.Printf("%v -> No Solution\n", vector)
		}
	}
}

func calc(nums []*Number, target float32) (*Number, bool) {
	N := len(nums)
	if N == 1 {
		if nums[0].value == target {
			return nums[0], true
		}
		return nil, false
	}
	for i := 0; i < N; i++ {
		for j := i + 1; j < N; j++ {
			reduced := make([]*Number, N)
			copy(reduced, nums)
			reduced[i] = reduced[N-1]
			reduced[j] = reduced[N-2]
			reduced = reduced[:N-1]
			for _, x := range combine(nums[i], nums[j]) {
				reduced[N-2] = x
				result, found := calc(reduced, target)
				if found {
					return result, found
				}
			}
		}
	}
	return nil, false
}

func combine(num1, num2 *Number) []*Number {
	var result []*Number
	result = append(result, CreateNumber(num1, num2, Plus))
	result = append(result, CreateNumber(num1, num2, Multiply))
	if num1.value > num2.value {
		result = append(result, CreateNumber(num1, num2, Minus))
	} else {
		result = append(result, CreateNumber(num2, num1, Minus))
	}
	if num2.value != 0 {
		result = append(result, CreateNumber(num1, num2, Divide))
	}
	if num1.value != 0 {
		result = append(result, CreateNumber(num2, num1, Divide))
	}
	return result
}
