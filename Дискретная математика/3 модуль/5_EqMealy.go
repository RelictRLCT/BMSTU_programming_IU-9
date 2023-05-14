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

	var n_first, m_first, q0_first, n_second, m_second, q0_second int

	in := bufio.NewReader(os.Stdin)

	fmt.Fscan(in, &n_first, &m_first, &q0_first)

	D_first := make([][]int, n_first)
	for i := 0; i < n_first; i++ {
		D_first[i] = make([]int, m_first)
	}

	F_first := make([][]string, n_first)
	for i := 0; i < n_first; i++ {
		F_first[i] = make([]string, m_first)
	}

	for i := 0; i < n_first; i++ {
		for j := 0; j < m_first; j++ {
			fmt.Fscan(in, &D_first[i][j])
		}
	}

	for i := 0; i < n_first; i++ {
		for j := 0; j < m_first; j++ {
			fmt.Fscan(in, &F_first[i][j])
		}
	}

	//2
	fmt.Fscan(in, &n_second, &m_second, &q0_second)
	D_second := make([][]int, n_second)
	for i := 0; i < n_second; i++ {
		D_second[i] = make([]int, m_second)
	}

	F_second := make([][]string, n_second)
	for i := 0; i < n_second; i++ {
		F_second[i] = make([]string, m_second)
	}

	for i := 0; i < n_second; i++ {
		for j := 0; j < m_second; j++ {
			fmt.Fscan(in, &D_second[i][j])
		}
	}

	for i := 0; i < n_second; i++ {
		for j := 0; j < m_second; j++ {
			fmt.Fscan(in, &F_second[i][j])
		}
	}

	used_first := make([]bool, n_first)
	numeration_first := make([]int, n_first)
	numbers_first := make([]int, n_first)

	for i := 0; i < n_first; i++ {
		numbers_first[i] = n_first - 1
	}

	dfs_canon(q0_first, used_first, numeration_first, D_first, F_first, m_first, numbers_first)

	D2_first := make([][]int, n_first)
	for i := 0; i < n_first; i++ {
		D2_first[i] = make([]int, m_first)
	}

	F2_first := make([][]string, n_first)
	for i := 0; i < n_first; i++ {
		F2_first[i] = make([]string, m_first)
	}

	for i := 0; i < n_first; i++ {
		for j := 0; j < m_first; j++ {
			D2_first[i][j] = numbers_first[D_first[numeration_first[i]][j]]
		}
	}

	for i := 0; i < n_first; i++ {
		for j := 0; j < m_first; j++ {
			F2_first[i][j] = F_first[numeration_first[i]][j]
		}
	}

	g_first := make([]Vertex, n_first)

	graph(&g_first, D2_first, F2_first, n_first, m_first)

	for i := 0; i < n_first; i++ {
		g_first[i].i = i
		g_first[i].number = i
	}

	D1_first, F1_first, m1_first := AufHohn(&g_first, n_first, m_first, D2_first, F2_first)

	g1_first := make([]Vertex, n_first)

	used2_first := make([]bool, m1_first)
	numeration2_first := make([]int, m1_first)
	numbers2_first := make([]int, m1_first)

	num = 0

	dfs_canon(0, used2_first, numeration2_first, D1_first, F1_first, m_first, numbers2_first)

	D1_end_first := make([][]int, m1_first)
	for i := 0; i < m1_first; i++ {
		D1_end_first[i] = make([]int, m_first)
	}

	F1_end_first := make([][]string, m1_first)
	for i := 0; i < m1_first; i++ {
		F1_end_first[i] = make([]string, m_first)
	}

	for i := 0; i < m1_first; i++ {
		for j := 0; j < m_first; j++ {
			D1_end_first[i][j] = numbers2_first[D1_first[numeration2_first[i]][j]]
		}
	}

	for i := 0; i < m1_first; i++ {
		for j := 0; j < m_first; j++ {
			F1_end_first[i][j] = F1_first[numeration2_first[i]][j]
		}
	}

	graph(&g1_first, D1_end_first, F1_end_first, m1_first, m_first)

	//2
	used_second := make([]bool, n_second)
	numeration_second := make([]int, n_second)
	numbers_second := make([]int, n_second)

	for i := 0; i < n_second; i++ {
		numbers_second[i] = n_second - 1
	}

	num = 0
	dfs_canon(q0_second, used_second, numeration_second, D_second, F_second, m_second, numbers_second)

	D2_second := make([][]int, n_second)
	for i := 0; i < n_second; i++ {
		D2_second[i] = make([]int, m_second)
	}

	F2_second := make([][]string, n_second)
	for i := 0; i < n_second; i++ {
		F2_second[i] = make([]string, m_second)
	}

	for i := 0; i < n_second; i++ {
		for j := 0; j < m_second; j++ {
			D2_second[i][j] = numbers_second[D_second[numeration_second[i]][j]]
		}
	}

	for i := 0; i < n_second; i++ {
		for j := 0; j < m_second; j++ {
			F2_second[i][j] = F_second[numeration_second[i]][j]
		}
	}

	g_second := make([]Vertex, n_second)

	graph(&g_second, D2_second, F2_second, n_second, m_second)

	for i := 0; i < n_second; i++ {
		g_second[i].i = i
		g_second[i].number = i
	}

	D1_second, F1_second, m1_second := AufHohn(&g_second, n_second, m_second, D2_second, F2_second)

	g1_second := make([]Vertex, n_second)

	used2_second := make([]bool, m1_second)
	numeration2_second := make([]int, m1_second)
	numbers2_second := make([]int, m1_second)

	num = 0

	dfs_canon(0, used2_second, numeration2_second, D1_second, F1_second, m_second, numbers2_second)

	D1_end_second := make([][]int, m1_second)
	for i := 0; i < m1_second; i++ {
		D1_end_second[i] = make([]int, m_second)
	}

	F1_end_second := make([][]string, m1_second)
	for i := 0; i < m1_second; i++ {
		F1_end_second[i] = make([]string, m_second)
	}

	for i := 0; i < m1_second; i++ {
		for j := 0; j < m_second; j++ {
			D1_end_second[i][j] = numbers2_second[D1_second[numeration2_second[i]][j]]
		}
	}

	for i := 0; i < m1_second; i++ {
		for j := 0; j < m_second; j++ {
			F1_end_second[i][j] = F1_second[numeration2_second[i]][j]
		}
	}

	graph(&g1_second, D1_end_second, F1_end_second, m1_second, m_second)

	if m1_first != m1_second {
		fmt.Println("NOT EQUAL")
		os.Exit(0)
	}

	for i := 0; i < m1_first; i++ {
		leng_first := len(g1_first[i].vers)
		leng_second := len(g1_second[i].vers)
		if leng_first != leng_second {
			fmt.Println("NOT EQUAL")
			os.Exit(0)
		}
		for j := 0; j < leng_first; j++ {
			if g1_first[i].vers[j].A != g1_second[i].vers[j].A {
				fmt.Println("NOT EQUAL")
				os.Exit(0)
			}

			if g1_first[i].vers[j].B != g1_second[i].vers[j].B {
				fmt.Println("NOT EQUAL")
				os.Exit(0)
			}

			if g1_first[i].vers[j].exit != g1_second[i].vers[j].exit {
				fmt.Println("NOT EQUAL")
				os.Exit(0)
			}
		}
	}

	fmt.Println("EQUAL")
	os.Exit(0)

	fmt.Fprintln(os.Stdout, "digraph {")
	fmt.Fprintln(os.Stdout, "    rankdir = LR")
	for i := 0; i < m1_first; i++ {
		leng := len(g1_first[i].vers)
		for j := 0; j < leng; j++ {
			fmt.Fprint(os.Stdout, "    ", g1_first[i].vers[j].A)
			fmt.Fprint(os.Stdout, " -> ")
			fmt.Fprint(os.Stdout, g1_first[i].vers[j].B)
			fmt.Fprint(os.Stdout, " [label = \"", g1_first[i].vers[j].sig, "(", g1_first[i].vers[j].exit, ")\"]")
			fmt.Fprintln(os.Stdout)
		}
	}
	fmt.Fprint(os.Stdout, "}")
	fmt.Fprintln(os.Stdout)
}
