package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"time"
)

type number struct {
	value float32
	expr  string
}

type pair struct {
	taken    []number
	nontaken []number
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
outer:
	for i := 0; i < 1000; i++ {
		var vector []int
		for j := 0; j < 4; j++ {
			vector = append(vector, rand.Intn(13)+1)
		}
		var nums []number
		for _, v := range vector {
			nums = append(nums, number{float32(v), strconv.Itoa(v)})
		}
		for _, result := range calc(nums) {
			if result.value == 24 {
				fmt.Printf("%v -> %s\n", vector, result.expr)
				continue outer
			}
		}
		fmt.Printf("%v -> No Solution\n", vector)
	}
}

func calc(nums []number) []number {
	if len(nums) == 1 {
		return nums
	}
	var result []number
	for _, list := range reduce(nums) {
		result = append(result, calc(list)...)
	}
	return result
}

func reduce(nums []number) [][]number {
	var result [][]number
	for _, v := range split(nums, 2) {
		for _, x := range combine2(v.taken) {
			result = append(result, append(v.nontaken, x))
		}
	}
	return result
}

func split(nums []number, n int) []pair {
	if n == 0 {
		return []pair{{[]number{}, nums}}
	}
	if len(nums) <= n {
		return []pair{{nums, []number{}}}
	}
	var result []pair
	for _, v := range split(nums[1:], n) {
		result = append(result, pair{v.taken, append(v.nontaken, nums[0])})
	}
	for _, v := range split(nums[1:], n-1) {
		result = append(result, pair{append(v.taken, nums[0]), v.nontaken})
	}
	return result
}

func combine2(nums []number) []number {
	result := []number{
		{nums[0].value + nums[1].value, fmt.Sprintf("(%s + %s)", nums[0].expr, nums[1].expr)},
		{nums[0].value * nums[1].value, fmt.Sprintf("(%s * %s)", nums[0].expr, nums[1].expr)},
	}
	if nums[0].value > nums[1].value {
		result = append(result, number{nums[0].value - nums[1].value,
			fmt.Sprintf("(%s - %s)", nums[0].expr, nums[1].expr)})
	} else {
		result = append(result, number{nums[1].value - nums[0].value,
			fmt.Sprintf("(%s - %s)", nums[1].expr, nums[0].expr)})
	}
	if nums[1].value != 0 {
		result = append(result, number{nums[0].value / nums[1].value,
			fmt.Sprintf("(%s / %s)", nums[0].expr, nums[1].expr)})
	}
	if nums[0].value != 0 {
		result = append(result, number{nums[1].value / nums[0].value,
			fmt.Sprintf("(%s / %s)", nums[1].expr, nums[0].expr)})
	}
	return result
}
