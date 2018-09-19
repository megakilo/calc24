package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"time"
)

// Number stores the value, the calcuation process and the last operator
type Number struct {
	value float32
	expr  string
	op    rune
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
			nums = append(nums, Number{float32(v), strconv.Itoa(v), 'x'})
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
	if len(nums) == 1 {
		if nums[0].value == target {
			return nums[0].expr, true
		}
		return "", false
	}
	for i := 0; i < len(nums); i++ {
		for j := i + 1; j < len(nums); j++ {
			reduced := make([]Number, len(nums))
			copy(reduced, nums)
			reduced[i] = reduced[len(reduced)-1]
			reduced[j] = reduced[len(reduced)-2]
			reduced = reduced[:len(reduced)-2]
			for _, x := range combine(nums[i], nums[j]) {
				reduced = append(reduced, x)
				result, found := calc(reduced, target)
				if found {
					return result, found
				}
				reduced = reduced[:len(reduced)-1]
			}
		}
	}
	return "", false
}

func combine(num1, num2 Number) []Number {
	var result []Number
	result = append(result, Number{num1.value + num2.value,
		fmt.Sprintf("%s + %s", num1.expr, num2.expr), '+'})
	result = append(result, Number{num1.value * num2.value,
		fmt.Sprintf("%s * %s", addParentheses(num1, "+-"), addParentheses(num2, "+-")), '*'})
	if num1.value > num2.value {
		result = append(result, Number{num1.value - num2.value,
			fmt.Sprintf("%s - %s", num1.expr, addParentheses(num2, "+-")), '-'})
	} else {
		result = append(result, Number{num2.value - num1.value,
			fmt.Sprintf("%s - %s", num2.expr, addParentheses(num1, "+-")), '-'})
	}
	if num2.value != 0 {
		result = append(result, Number{num1.value / num2.value,
			fmt.Sprintf("%s / %s", addParentheses(num1, "+-"), addParentheses(num2, "+-*")), '/'})
	}
	if num1.value != 0 {
		result = append(result, Number{num2.value / num1.value,
			fmt.Sprintf("%s / %s", addParentheses(num2, "+-"), addParentheses(num1, "+-*")), '/'})
	}
	return result
}

func addParentheses(x Number, conditions string) string {
	for _, v := range conditions {
		if v == x.op {
			return "(" + x.expr + ")"
		}
	}
	return x.expr
}
