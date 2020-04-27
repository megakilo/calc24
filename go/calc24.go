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

// Number stores the value, the calcuation process and the last operator
type Number struct {
	value float32
	expr  string
	op    OpType
}

func AddParentheses(x Number, is_denominator bool) string {
	if x.op == Plus || x.op == Minus || (is_denominator && x.op != None) {
			return fmt.Sprintf("(%s)", x.expr)
	}
	return x.expr
}

func CreateNumber(num1, num2 Number, op OpType) Number {
	var value float32
	var expr string
	switch op {
	case Plus:
		value = num1.value + num2.value
		expr = fmt.Sprintf("%s + %s", num1.expr, num2.expr)
	case Minus:
		value = num1.value - num2.value
		expr = fmt.Sprintf("%s - %s", num1.expr, AddParentheses(num2, false))
	case Multiply:
		value = num1.value * num2.value
		expr = fmt.Sprintf("%s * %s", AddParentheses(num1, false), AddParentheses(num2, false))
	case Divide:
		value = num1.value / num2.value
		expr = fmt.Sprintf("%s / %s", AddParentheses(num1, false), AddParentheses(num2, true))
	}
	return Number {value, expr, op}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	for i := 0; i < 1000; i++ {
		var vector []int
		for j := 0; j < 4; j++ {
			vector = append(vector, rand.Intn(13)+1)
		}
		var nums []Number
		for _, v := range vector {
			nums = append(nums, Number{float32(v), strconv.Itoa(v), None})
		}
		result, found := calc(nums, 24)
		if found {
			fmt.Printf("%v -> %s\n", vector, result)
		} else {
			fmt.Printf("%v -> No Solution\n", vector)
		}
	}
}

func calc(nums []Number, target float32) (string, bool) {
	N := len(nums)
	if N == 1 {
		if nums[0].value == target {
			return nums[0].expr, true
		}
		return "", false
	}
	for i := 0; i < N; i++ {
		for j := i + 1; j < N; j++ {
			reduced := make([]Number, N)
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
	return "", false
}

func combine(num1, num2 Number) []Number {
	var result []Number
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
