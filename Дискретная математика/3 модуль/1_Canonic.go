package main

import (
	"fmt"
)

type Vertex struct {
	vers   []Edge
	number int
}

type Edge struct {
	A, B int
	sig  string
	exit string
}

func graph(g *[]Vertex, D [][]int, F [][]string, n int, m int) {
	var q0 = 0
	for i := 0; i < n; i++ {
		q0 = i
		for j := 0; j < m; j++ {
			exit := F[q0][j]
			q := D[q0][j]

			var e Edge
			e.A = q0
			e.B = q
			e.sig = string(j + 'a')
			e.exit = exit

			(*g)[q0].vers = append((*g)[q0].vers, e)
		}
	}

}

var num = 0

func dfs_canon(v int, g *[]Vertex, used []bool, numeration []int) {
	(*g)[v].number = num
	numeration[num] = v
	num += 1
	used[v] = true
	verlen := len((*g)[v].vers)
	for i := 0; i < verlen; i++ {
		to := (*g)[v].vers[i].B
		if !used[to] {
			dfs_canon(to, g, used, numeration)
		}
	}
}

func main() {
	var n, m, q0 int
	fmt.Scan(&n, &m, &q0)

	D := make([][]int, n)
	for i := 0; i < n; i++ {
		D[i] = make([]int, m)
	}

	F := make([][]string, n)
	for i := 0; i < n; i++ {
		F[i] = make([]string, m)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Scan(&D[i][j])
		}
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Scan(&F[i][j])
		}
	}

	g := make([]Vertex, n)

	for i := 0; i < n; i++ {
		g[i].number = n - 1
	}

	graph(&g, D, F, n, m)

	used := make([]bool, n)

	numeration := make([]int, n)

	dfs_canon(q0, &g, used, numeration)

	fmt.Println(n, m, g[q0].number)
	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Print(g[D[numeration[i]][j]].number, " ")
		}
		fmt.Println()
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Print(F[numeration[i]][j], " ")
		}
		fmt.Println()
	}
}
