package main

import "fmt"

type graph struct {
	N     int
	M     [][]int
	color []string
}

func dfs_find(v int, graph *graph, used *[]bool, length *int) {
	(*used)[v] = true
	*length += 1
	for i := 0; i < len(graph.M[v]); i++ {
		to := graph.M[v][i]
		if !(*used)[to] {
			dfs_find(to, graph, used, length)
		}
	}
}

func find_components(graph *graph) []int { // https://e-maxx.ru/algo/connected_components
	len := 0
	used := make([]bool, graph.N)
	maxs := make([]int, 1)
	max := 0
	maxlen := 0

	for i := 0; i < graph.N; i++ {
		used[i] = false
	}

	for i := 0; i < graph.N; i++ {
		len = 0
		if !used[i] {
			dfs_find(i, graph, &used, &len)
			if len > maxlen {
				maxlen = len
				max = i
				maxs = maxs[:1]
				maxs[0] = max
			} else if len == maxlen {
				maxs = append(maxs, i)
			}
		}
	}
	return maxs
}

func dfs_edges(graph *graph, v int, used *[]bool, edges *int) {
	(*used)[v] = true
	for i := 0; i < len(graph.M[v]); i++ {
		to := graph.M[v][i]
		if to == v {
			for j := 0; j < len(graph.M[v]); j++ {
				if graph.M[v][j] == to {
					*edges += 1
				}
			}
		} else {
			*edges += 1
		}
		if !(*used)[to] {
			dfs_edges(graph, to, used, edges)
		}
	}
}

func dfs_color(graph *graph, v int, used *[]bool) {
	(*used)[v] = true
	for i := 0; i < len(graph.M[v]); i++ {
		to := graph.M[v][i]
		graph.color[graph.M[v][i]] = "RED"
		if !(*used)[to] {
			dfs_color(graph, to, used)
		}
	}
}

func color(graph *graph, v []int) {
	maxs := make([]int, 1)
	used := make([]bool, graph.N)
	for i := 0; i < graph.N; i++ {
		used[i] = false
	}
	if len(v) > 1 {
		count := 0
		for i := 0; i < len(v); i++ {
			usedd := make([]bool, graph.N)
			for j := 0; j < graph.N; j++ {
				usedd[j] = false
			}
			edges := 0
			dfs_edges(graph, v[i], &usedd, &edges)
			edges /= 2
			if edges > count {
				count = edges
				maxs = maxs[:1]
				maxs[0] = v[i]
			} else if edges == count {
				maxs = append(maxs, v[i])
			}
		}
	} else {
		dfs_color(graph, v[0], &used)
		return
	}

	min := 1000000
	if len(maxs) > 1 {
		for i := 0; i < len(maxs); i++ {
			if maxs[i] < min {
				min = maxs[i]
			}
		}
	} else {
		dfs_color(graph, maxs[0], &used)
		return
	}

	dfs_color(graph, min, &used)
}

func main() {
	var N, M int
	fmt.Scan(&N, &M)

	if N == 0 {
		fmt.Println("graph {")
		fmt.Print("}")
		return
	}
	var graph graph
	graph.N = N
	graph.M = make([][]int, N)
	graph.color = make([]string, N)

	for i := 0; i < M; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		if u != v {
			graph.M[u] = append(graph.M[u], v)
			graph.M[v] = append(graph.M[v], u)
		} else {
			graph.M[u] = append(graph.M[u], v)
		}
	}

	color(&graph, find_components(&graph))

	if M == 0 {
		graph.color[0] = "RED"
	}

	fmt.Println("graph {")
	for i := 0; i < graph.N; i++ {
		if graph.color[i] == "RED" {
			fmt.Print("   ", i)
			fmt.Println(" [color=red]")
		} else {
			fmt.Println("  ", i)
		}
	}

	for i := 0; i < graph.N; i++ {
		for j := 0; j < len(graph.M[i]); j++ {
			if graph.M[i][j] >= i {
				if graph.color[i] == "RED" {
					fmt.Print("   ", i, " -- ", graph.M[i][j])
					fmt.Println(" [color=red]")
				} else {
					fmt.Println("  ", i, "--", graph.M[i][j])
				}
			}
		}
	}

	fmt.Print("}")
}
