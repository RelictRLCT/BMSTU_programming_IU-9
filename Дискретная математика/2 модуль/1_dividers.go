package main

import (
	"fmt"
	"sort"
)

func divs(x int) []int {
	divs := make([]int, 0)
	for i := 1; i*i <= x; i++ {
		if x%i == 0 {
			if i*i != x {
				divs = append(divs, i)
			}

			divs = append(divs, x/i)
		}
	}

	sort.Slice(divs, func(i int, j int) bool { return divs[j] < divs[i] })

	return divs
}

func main() {
	var x int
	fmt.Scan(&x)
	div := divs(x)
	fmt.Println("graph {")
	for _, y := range div {
		fmt.Println("   ", y)
	}

	for i, y := range div {
		for j := i + 1; j < len(div); j++ {
			if y%div[j] == 0 {
				for _, x2 := range div {
					if x2 != y && x2 != div[j] && y%x2 == 0 && x2%div[j] == 0 {
						goto A
					}
				}
				fmt.Println("   ", y, "--", div[j])
			}
		A:
		}
	}

	fmt.Print("}")

}
