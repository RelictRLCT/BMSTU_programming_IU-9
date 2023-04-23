package main

import (
	"fmt"
	"sort"
)

type Vertex struct {
	vers []Edge
}

type Edge struct {
	A, B int
	sig  string
	exit string
}

var count = 0
var count2 = 0

func grapf(g *[]Vertex, D [][]int, F [][]string, n int, m int, q0 int, M int) {
	q0 = 0
	for count2 < n {
		count = 0
		for count < m {
			exit := F[q0][count]
			q := D[q0][count]

			var e Edge
			e.A = q0
			e.B = q
			e.sig = string(count + 'a')
			e.exit = exit

			(*g)[q0].vers = append((*g)[q0].vers, e)

			count += 1
		}
		count2 += 1
		q0 = count2
	}
}

func dfs_find(v int, n int, word string, graph *[]Vertex, length int, M int, set *map[string]struct{}, tsikllambdaguard int) {
	if length <= M {
		(*set)[word] = struct{}{}
	}
	lenvers := len((*graph)[v].vers)
	for i := 0; i < lenvers; i++ {
		to := (*graph)[v].vers[i].B
		exit := (*graph)[v].vers[i].exit
		if exit != "-" {
			if length <= M {
				dfs_find(to, n, word+exit, graph, length+1, M, set, 0)
			}
		} else if tsikllambdaguard == 0 && length <= M {
			dfs_find(to, n, word, graph, length, M, set, tsikllambdaguard+1)
		}
	}
}

func findwords(g *[]Vertex, n int, q0 int, M int, set *map[string]struct{}) {
	dfs_find(q0, n, "", g, 0, M, set, 0)
}

func main() {
	var n, m, q0, M int
	fmt.Scan(&n)

	m = 2

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

	fmt.Scan(&q0, &M)

	g := make([]Vertex, n)

	grapf(&g, D, F, n, m, q0, M)

	var set = make(map[string]struct{})

	findwords(&g, n, q0, M, &set)

	words := make([]string, 0)

	for i := range set {
		words = append(words, i)
	}

	sort.Strings(words)

	for _, word := range words {
		if word != "" {
			fmt.Print(word, " ")
		}
	}
}
