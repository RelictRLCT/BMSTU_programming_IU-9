package main

import (
	"fmt"
	"os"
)

type drob struct {
	chis int
	znam int
}

func gcd(a, b int) int {
	a2 := a
	b2 := b

	if b2 < 0 {
		b2 = -b2
	}

	if a2 < 0 {
		a2 = -a2
	}

	if b2 == 0 {
		return a2
	}
	return gcd(b2, a2%b2)
}

func lcm(a, b int) int {
	return (a * b) / gcd(a, b)
}

func make_drob(a int) drob {
	var b drob
	b.chis = a
	b.znam = 1
	return b
}

func reduce(a drob) drob {
	red := a
	var grand int
	grand = gcd(red.znam, red.chis)
	for grand != 1 {
		red.znam /= grand
		red.chis /= grand
		grand = gcd(red.znam, red.chis)
	}
	return red
}

func add_drob(a, b drob) drob {
	var sum drob
	if a.znam == b.znam {
		sum.chis = a.chis + b.chis
		sum.znam = a.znam
	} else {
		krat := lcm(a.znam, b.znam)
		a.chis *= krat / a.znam
		b.chis *= krat / b.znam
		a.znam *= krat / a.znam
		b.znam *= krat / b.znam
		sum.chis = a.chis + b.chis
		sum.znam = a.znam
	}
	sum = reduce(sum)
	return sum
}

func sub_drob(a, b drob) drob {
	b2 := b
	b2.chis = -b2.chis
	return add_drob(a, b2)
}

func mul_drob(a, b drob) drob {
	var res drob
	res.chis = a.chis * b.chis
	res.znam = a.znam * b.znam
	res = reduce(res)
	return res
}

func dev_drob(a, b drob) drob {
	var res, b2 drob
	b2 = b
	b2.chis, b2.znam = b2.znam, b2.chis
	if b2.znam < 0 {
		b2.chis *= -1
		b2.znam *= -1
	}
	res = mul_drob(a, b2)
	return res
}

func out_drob(a drob) {
	fmt.Print(a.chis)
	fmt.Print("/")
	fmt.Print(a.znam)
	fmt.Print(" ")
}
func solutions(A [][]drob, n int) {
	for i := range A {
		diag := A[i][i]
		if diag.chis == 0 {
			swap := false
			for g := i; g < n; g++ {
				if A[g][i].chis != 0 {
					swap = true
					A[i], A[g] = A[g], A[i]
					break
				}
			}
			if swap == false {
				fmt.Print("No solution")
				os.Exit(0)
			}
			diag = A[i][i]
		}

		for j := i + 1; j < n; j++ {
			norm := dev_drob(A[j][i], diag)
			for k := 0; k < n+1; k++ {
				A[j][k] = sub_drob(A[j][k], mul_drob(A[i][k], norm))
			}
		}
	}

	/*for i, _ := range A { //вывод матрицы, приведённой к треугольному виду
		for j, _ := range A[i] {
			out_drob(A[i][j])
		}
		fmt.Println()
	}*/

	res := make([]drob, n)
	for i := n - 1; i >= 0; i-- {
		sum := make_drob(0)
		for j := i + 1; j < n; j++ {
			sum = add_drob(sum, mul_drob(A[i][j], res[j]))
		}
		res[i] = dev_drob(sub_drob(A[i][n], sum), A[i][i])
	}

	for i := range res {
		out_drob(res[i])
		fmt.Println()
	}

}

func main() {
	var n int
	fmt.Scanf("%d", &n)

	A := make([][]int, n)

	for i := range A {
		A[i] = make([]int, n+1)
	}

	for i := range A {
		for j := range A[i] {
			fmt.Scan(&A[i][j])
		}
	}
	var B [][]drob
	B = make([][]drob, n)

	for i := range B {
		B[i] = make([]drob, n+1)
	}

	for i := range A {
		for j := range A[i] {
			B[i][j] = make_drob(A[i][j])
		}
	}
	solutions(B, n)
}
