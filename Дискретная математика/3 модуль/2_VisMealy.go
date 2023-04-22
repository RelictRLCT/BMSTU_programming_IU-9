package main

import "fmt"

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

func grapf(g *[]Vertex, D [][]int, F [][]string, n int, m int, q0 int) {
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

	grapf(&g, D, F, n, m, q0)

	fmt.Println("digraph {")
	fmt.Println("    rankdir = LR")
	for i := 0; i < n; i++ {
		leng := len(g[i].vers)
		for j := 0; j < leng; j++ {
			fmt.Print("    ", g[i].vers[j].A)
			fmt.Print(" -> ")
			fmt.Print(g[i].vers[j].B)
			fmt.Print(" [label = \"", g[i].vers[j].sig, "(", g[i].vers[j].exit, ")\"]")
			fmt.Println()
		}
	}
	fmt.Print("}")
}
