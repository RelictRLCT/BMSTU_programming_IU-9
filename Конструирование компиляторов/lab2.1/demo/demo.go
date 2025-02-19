package main

import "fmt"

func test(a, b, c, d int) {
	fmt.Println(a + b)
}

func test2(a, b, c, d float32) {
	fmt.Println(a + b)
}

func test3(a int, b int) {
	fmt.Println(a + b)
}

func main() {
	test(4, 8, 0, 0)
}
