package main

import (
	"fmt"
)

type Vertex struct {
	vers   []*Vertex
	T1     int
	comp   int
	low    int
	number int
}

type Edge struct {
	A, B int
}

type Stack struct {
	cap  int
	top  int
	data []*Vertex
}

var (
	time  = 1
	count = 1
)

func InitStack(n int) Stack {
	var s Stack
	s.top = 0
	s.data = make([]*Vertex, n)
	s.cap = n
	return s
}

func Stackempty(s Stack) bool {
	return s.top == 0
}

func Push(s *Stack, v *Vertex) {
	(*s).data[(*s).top] = v
	(*s).top += 1
}

func Pop(s *Stack) *Vertex {
	v := (*s).data[(*s).top-1]
	(*s).top -= 1
	return v
}

func VisitVertex(g *[]Vertex, v *Vertex, s *Stack) {
	(*v).T1 = time
	(*v).low = time
	time += 1
	Push(s, v)
	for _, u := range (*v).vers {
		if u.T1 == 0 {
			VisitVertex(g, u, s)
		}
		if u.comp == 0 && v.low > u.low {
			v.low = u.low
		}
	}
	if v.T1 == v.low {
		for true {
			u := Pop(s)
			u.comp = count
			if u == v {
				break
			}
		}
		count += 1
	}
}

func Tarjan(g *[]Vertex, n int) {
	for i := 0; i < n; i++ {
		(*g)[i].T1 = 0
		(*g)[i].comp = 0
	}

	s := InitStack(n + 1)

	for i := 0; i < n; i++ {
		if (*g)[i].T1 == 0 {
			VisitVertex(g, &(*g)[i], &s)
		}
	}
}

func Condens(super *[]Vertex, g *[]Vertex) []Edge {
	edges := make([]Edge, 0)
	for _, v := range *g {
		for _, u := range v.vers {
			if v.comp != u.comp {
				(*super)[v.comp-1].vers = append((*super)[v.comp-1].vers, &((*super)[u.comp-1]))
				var e Edge
				e.A = v.comp - 1
				e.B = u.comp - 1
				edges = append(edges, e)
			}
		}
	}
	return edges
}

func Это_БАЗА(s *[]Vertex, edges *[]Edge) []int {
	base := make([]int, 0)
	for i, _ := range *s {
		for _, e := range *edges {
			if i == e.B {
				goto A
			}
		}
		base = append(base, i+1)
	A:
	}
	return base
}

func main() {
	var n, m int
	fmt.Scan(&n, &m)
	g := make([]Vertex, n)

	for i := 0; i < n; i++ {
		g[i].number = i
	}
	for i := 0; i < m; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		g[u].vers = append(g[u].vers, &g[v])
	}

	Tarjan(&g, n)

	superg := make([]Vertex, count-1)

	edges := Condens(&superg, &g)

	base := Это_БАЗА(&superg, &edges)

	for _, x := range base {
		min := n
		for _, v := range g {
			if v.comp == x {
				if v.number < min {
					min = v.number
				}
			}
		}

		fmt.Println(min)
	}
}
