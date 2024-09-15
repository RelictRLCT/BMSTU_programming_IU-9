package main

import (
	"fmt"
	"math"
	"math/rand"
	"sync"
	"time"
)

func print_matrixs(n int, m1 [][]int, m2 [][]int, res [][]int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			print(m1[i][j], " ")
		}
		println()
	}
	println()
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			print(m2[i][j], " ")
		}
		println()
	}
	println()

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			print(res[i][j], " ")
		}
		println()
	}
}

func multipl_matrix_by_row(n int, m1 [][]int, m2 [][]int, res [][]int) {
	for resi := 0; resi < n; resi++ {
		for resj := 0; resj < n; resj++ {
			for i := 0; i < n; i++ {
				res[resi][resj] += m1[resi][i] * m2[i][resj]
			}
		}
	}
}

func multipl_matrix_by_column(n int, m1 [][]int, m2 [][]int, res [][]int) {
	for resi := 0; resi < n; resi++ {
		for resj := 0; resj < n; resj++ {
			for i := 0; i < n; i++ {
				res[resj][resi] += m1[resj][i] * m2[i][resi]
			}
		}
	}
}

func clear_matrix(res [][]int) {
	for i := 0; i < len(res); i++ {
		for j := 0; j < len(res[i]); j++ {
			res[i][j] = 0
		}
	}
}

func multipl_matrix_with_go(n int, m1 [][]int, m2 [][]int, res [][]int, count_of_squares int) {
	var wg sync.WaitGroup
	wg.Add(count_of_squares)

	count_of_squares_in_row := int(math.Sqrt(float64(count_of_squares)))
	mul := func(r, c int) {
		defer wg.Done()

		for resi := r * n / count_of_squares_in_row; resi < (r+1)*n/count_of_squares_in_row; resi++ {
			for resj := c * n / count_of_squares_in_row; resj < (c+1)*n/count_of_squares_in_row; resj++ {
				for i := 0; i < n; i++ {
					res[resi][resj] += m1[resi][i] * m2[i][resj]
				}
			}
		}

	}

	for i := 0; i < count_of_squares_in_row; i++ {
		for j := 0; j < count_of_squares_in_row; j++ {
			go mul(i, j)
		}
	}
	wg.Wait()
}

func main() {
	n := 2000
	m1 := make([][]int, n)
	m2 := make([][]int, n)
	res := make([][]int, n)

	for i := 0; i < n; i++ {
		m1[i] = make([]int, n)
		m2[i] = make([]int, n)
		res[i] = make([]int, n)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			m1[i][j] = rand.Int() % 100
			m2[i][j] = rand.Int() % 100
		}
	}

	//start := time.Now()
	//multipl_matrix_by_row(n, m1, m2, res)
	//end := time.Now()
	//fmt.Println("По строкам :", end.Sub(start).Seconds())
	//
	////print_matrixs(n, m1, m2, res)
	//clear_matrix(res)
	//
	//start = time.Now()
	//multipl_matrix_by_column(n, m1, m2, res)
	//end = time.Now()
	//fmt.Println("По столбцам: ", end.Sub(start).Seconds())

	//print_matrixs(n, m1, m2, res)
	clear_matrix(res)
	start := time.Now()
	multipl_matrix_with_go(n, m1, m2, res, 36)
	end := time.Now()
	fmt.Println("Параллельно :", end.Sub(start).Seconds())
	//print_matrixs(n, m1, m2, res)

}
