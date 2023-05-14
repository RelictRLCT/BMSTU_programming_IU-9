package main

import (
	"bufio"
	"fmt"
	"os"
)

type Les struct {
	x      Vertex
	parent *Les
}

func Union(x *Vertex, y *Vertex) {
	rootx := Find(x)
	rooty := Find(y)
	rootx.parent = rooty
}

func Find(x *Vertex) *Vertex {
	var root *Vertex
	if x.parent == x {
		root = x
	} else {
		root = Find(x.parent)
	}
	return root
}

type Vertex struct {
	vers   []Edge
	number int
	i      int
	depth  int
	parent *Vertex
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

func dfs_canon(v int, used []bool, numeration []int, D [][]int, F [][]string, m int, numbers []int) {
	numeration[num] = v
	numbers[v] = num
	num += 1
	used[v] = true
	for i := 0; i < m; i++ {
		to := D[v][i]
		if !used[to] {
			dfs_canon(to, used, numeration, D, F, m, numbers)
		}
	}
}

func dfs_canon2(v int, used []bool, numeration []int, D [][]int, F [][]string, m int, numbers []int) {
	numeration[num] = v
	numbers[v] = num
	num += 1
	used[v] = true
	for i := 0; i < m; i++ {
		to := D[v][i]
		if !used[to] {
			dfs_canon2(to, used, numeration, D, F, m, numbers)
		}
	}
}

func Split1(g *[]Vertex, n int, M_alf int, F [][]string, Pi []*Vertex) int {
	m := n
	for i := range *g {
		(*g)[i].parent = &(*g)[i]
		(*g)[i].depth = 0
	}
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			q1 := &(*g)[i]
			q2 := &(*g)[j]
			if Find(q1) != Find(q2) {
				eq := true
				for k := 0; k < M_alf; k++ {
					if F[q1.i][k] != F[q2.i][k] {
						eq = false
						break
					}
				}
				if eq {
					Union(q1, q2)
					m -= 1
				}
			}
		}
	}

	for _, q := range *g {
		Pi[q.i] = Find(&q)
	}

	return m
}

func Split(g *[]Vertex, n int, M_alf int, D [][]int, Pi []*Vertex) int {
	m := n
	for i := range *g {
		(*g)[i].parent = &(*g)[i]
		(*g)[i].depth = 0
	}

	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			q1 := &(*g)[i]
			q2 := &(*g)[j]
			if Pi[q1.i] == Pi[q2.i] && Find(q1) != Find(q2) {
				eq := true
				for k := 0; k < M_alf; k++ {
					w1 := D[q1.i][k]
					w2 := D[q2.i][k]
					if Pi[w1] != Pi[w2] {
						eq = false
						break
					}
				}
				if eq {
					Union(q1, q2)
					m -= 1
				}
			}
		}
	}

	for _, q := range *g {
		Pi[q.i] = Find(&q)
	}
	return m
}

func AufHohn(g *[]Vertex, n int, M_alf int, D [][]int, F [][]string) ([][]int, [][]string, int) {
	Pi := make([]*Vertex, n)
	m := Split1(g, n, M_alf, F, Pi)
	for true {
		m1 := Split(g, n, M_alf, D, Pi)
		if m == m1 {
			break
		}
		m = m1
	}

	D1 := make([][]int, n)
	F1 := make([][]string, n)

	for i := 0; i < n; i++ {
		D1[i] = make([]int, M_alf)
		F1[i] = make([]string, M_alf)
	}

	newnum := 0
	for i := 0; i < n; i++ {
		if Pi[i].i >= newnum {
			tmp := Pi[i].i
			for j := i; j < n; j++ {
				if Pi[j].i == tmp {
					Pi[j].i = newnum
				}
			}
			newnum += 1
		}
	}

	for i := range *g {
		q1 := Pi[(*g)[i].number]
		for j := 0; j < M_alf; j++ {
			D1[q1.i][j] = (*Pi[D[(*g)[i].number][j]]).i
			F1[q1.i][j] = F[(*g)[i].number][j]
		}
	}

	return D1, F1, m

}

func main() {

	var n, m, q0 int

	in := bufio.NewReader(os.Stdin)

	fmt.Fscan(in, &n, &m, &q0)

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
			fmt.Fscan(in, &D[i][j])
		}
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Fscan(in, &F[i][j])
		}
	}

	used := make([]bool, n)
	numeration := make([]int, n)
	numbers := make([]int, n)

	for i := 0; i < n; i++ {
		numbers[i] = n - 1
	}

	dfs_canon(q0, used, numeration, D, F, m, numbers)

	D2 := make([][]int, n)
	for i := 0; i < n; i++ {
		D2[i] = make([]int, m)
	}

	F2 := make([][]string, n)
	for i := 0; i < n; i++ {
		F2[i] = make([]string, m)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			D2[i][j] = numbers[D[numeration[i]][j]]
		}
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			F2[i][j] = F[numeration[i]][j]
		}
	}

	g := make([]Vertex, n)

	graph(&g, D2, F2, n, m)

	for i := 0; i < n; i++ {
		g[i].i = i
		g[i].number = i
	}

	D1, F1, m1 := AufHohn(&g, n, m, D2, F2)

	g1 := make([]Vertex, n)

	used2 := make([]bool, m1)
	numeration2 := make([]int, m1)
	numbers2 := make([]int, m1)

	num = 0

	dfs_canon(0, used2, numeration2, D1, F1, m, numbers2)

	D1_end := make([][]int, m1)
	for i := 0; i < m1; i++ {
		D1_end[i] = make([]int, m)
	}

	F1_end := make([][]string, m1)
	for i := 0; i < m1; i++ {
		F1_end[i] = make([]string, m)
	}

	for i := 0; i < m1; i++ {
		for j := 0; j < m; j++ {
			D1_end[i][j] = numbers2[D1[numeration2[i]][j]]
		}
	}

	for i := 0; i < m1; i++ {
		for j := 0; j < m; j++ {
			F1_end[i][j] = F1[numeration2[i]][j]
		}
	}

	graph(&g1, D1_end, F1_end, m1, m)

	fmt.Fprintln(os.Stdout, "digraph {")
	fmt.Fprintln(os.Stdout, "    rankdir = LR")
	for i := 0; i < m1; i++ {
		leng := len(g1[i].vers)
		for j := 0; j < leng; j++ {
			fmt.Fprint(os.Stdout, "    ", g1[i].vers[j].A)
			fmt.Fprint(os.Stdout, " -> ")
			fmt.Fprint(os.Stdout, g1[i].vers[j].B)
			fmt.Fprint(os.Stdout, " [label = \"", g1[i].vers[j].sig, "(", g1[i].vers[j].exit, ")\"]")
			fmt.Fprintln(os.Stdout)
		}
	}
	fmt.Fprint(os.Stdout, "}")
	fmt.Fprintln(os.Stdout)
}
