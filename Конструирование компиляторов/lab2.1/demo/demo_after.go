package main

import "fmt"

func test(a int, b int, c int, d int) {
	fmt.Println(a + b)
}

func test2(a float32, b float32, c float32, d float32) {
	fmt.Println(a + b)
}

func test3(a int, b int) {
	fmt.Println(a + b)
}

func main() {
	test(4, 8, 0, 0)
}
