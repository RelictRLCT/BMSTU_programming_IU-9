package main

import (
	"fmt"
)

type graph struct {
	N int
	M [][]int
}

var (
	count int = 0
	timer int
)

func min(a, b int) int {
	if a > b {
		return b
	}
	return a
}

func dfs(v, p int, used *[]bool, graph *graph, tin, fup *[]int) { 
	(*used)[v] = true
	timer++
	(*tin)[v], (*fup)[v] = timer, timer
	for i := 0; i < len(graph.M[v]); i++ {
		to := graph.M[v][i]
		if to == p {
			continue
		}
		if (*used)[to] {
			(*fup)[v] = min((*fup)[v], (*tin)[to])
		} else {
			dfs(to, v, used, graph, tin, fup)
			(*fup)[v] = min((*fup)[v], (*fup)[to])
			if (*fup)[to] > (*tin)[v] {
				count += 1
			}
		}
	}
}

func bridge(used *[]bool, graph *graph) {
	timer = 0
	tin := make([]int, graph.N)
	fup := make([]int, graph.N)
	for i := 0; i < graph.N; i++ {
		(*used)[i] = false
	}
	for i := 0; i < graph.N; i++ {
		if !(*used)[i] {
			dfs(i, -1, used, graph, &tin, &fup)
		}
	}
}

func main() {
	var N, M int
	fmt.Scan(&N, &M)

	var graph graph
	graph.N = N
	graph.M = make([][]int, N)

	for i := 0; i < M; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		graph.M[u] = append(graph.M[u], v)
		graph.M[v] = append(graph.M[v], u)
	}

	used := make([]bool, N)

	bridge(&used, &graph)
	fmt.Print(count)
}
