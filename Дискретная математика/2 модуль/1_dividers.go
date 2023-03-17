package main

import "fmt"

func divs(x int) []int {
	divs := make([]int, 1)
	divs[0] = x
	for i := x - 1; i >= 1; i-- {
		if x%i == 0 {
			divs = append(divs, i)
		}
	}
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
