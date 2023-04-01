package main

import (
	"fmt"
	"math"
	"sort"
)

type Les struct {
	x      int
	parent *Les
}

type Edge struct {
	A, B int
	ves  float64
}

type Atr struct {
	a, b int
}

func MakeSet(x int, root *Les) *Les {
	r := root
	r.x = x
	r.parent = root
	return r
}

func Union(x *Les, y *Les) {
	rootx := Find(x)
	rooty := Find(y)
	rootx.parent = rooty
}

func Find(x *Les) *Les {
	var root *Les
	if x.parent == x {
		root = x
	} else {
		root = Find(x.parent)
	}
	return root
}

func Kruskal(n int, e []Edge) []Edge {
	var res []Edge
	setles := make([]*Les, 0)
	for i := 0; i < n; i++ {
		var s = Les{i, nil}
		setles = append(setles, MakeSet(i, &s))
	}
	sort.Slice(e, func(i, j int) bool {
		return e[i].ves < e[j].ves
	})

	for _, ed := range e {
		if Find(setles[ed.A]) != Find(setles[ed.B]) {
			res = append(res, ed)
			Union(setles[ed.A], setles[ed.B])
		}
	}
	return res
}

func main() {
	var n int
	fmt.Scan(&n)
	g := make([]Edge, 0)
	atr := make([]Atr, n)
	for i := 0; i < n; i++ {
		var a, b int
		fmt.Scan(&a, &b)
		atr[i].a = a
		atr[i].b = b
	}
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			var e Edge
			e.A = i
			e.B = j
			e.ves = math.Sqrt(math.Pow(float64(atr[i].a-atr[j].a), 2) + math.Pow(float64(atr[i].b-atr[j].b), 2))
			g = append(g, e)
		}
	}

	res := Kruskal(n, g)
	sum := 0.0
	for i := 0; i < len(res); i++ {
		sum += res[i].ves
	}
	fmt.Printf("%.2f", sum)
}
