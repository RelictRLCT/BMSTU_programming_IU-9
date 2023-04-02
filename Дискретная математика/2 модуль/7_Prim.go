package main

import (
	"fmt"
)

type queue struct {
	heap       []*Vertex
	cap, count int
}

type Vertex struct {
	edges      []Edge
	key, index int
	value      *Vertex
}

type Edge struct {
	A, B, len int
}

func Initpqueue(n int) queue {
	var q queue
	q.heap = make([]*Vertex, n)
	q.cap = n
	q.count = 0
	return q
}

func QueueEmpty(q *queue) bool {
	return q.count == 0
}

func Insert(q *queue, v *Vertex) {
	i := q.count
	q.count = i + 1
	q.heap[i] = v
	for i > 0 && q.heap[(i-1)/2].key > q.heap[i].key {
		q.heap[(i-1)/2], q.heap[i] = q.heap[i], q.heap[(i-1)/2]
		q.heap[i].index = i
		i = (i - 1) / 2
	}
	q.heap[i].index = i
}

func Decreasekey(q *queue, v *Vertex, k int) {
	i := v.index
	v.key = k
	for i > 0 && q.heap[(i-1)/2].key > k {
		q.heap[(i-1)/2], q.heap[i] = q.heap[i], q.heap[(i-1)/2]
		q.heap[i].index = i
		i = (i - 1) / 2
	}
	v.index = i
}

func Heapify(i int, n int, P *[]*Vertex) {
	for true {
		l := 2*i + 1
		r := l + 1
		j := i
		if l < n && (*P)[i].key > (*P)[l].key {
			i = l
		}
		if r < n && (*P)[i].key > (*P)[r].key {
			i = r
		}
		if i == j {
			break
		}
		(*P)[i], (*P)[j] = (*P)[j], (*P)[i]
		(*P)[i].index = i
		(*P)[j].index = j
	}
}

func Extractmin(q *queue) *Vertex {
	v := q.heap[0]
	q.count -= 1
	if q.count > 0 {
		q.heap[0] = q.heap[q.count]
		q.heap[0].index = 0
		Heapify(0, q.count, &q.heap)
	}
	return v
}

func Prim(g *[]Vertex) []Vertex {
	res := make([]Vertex, 0)
	for i := 0; i < len(*g); i++ {
		(*g)[i].index = -1
	}
	q := Initpqueue(len(*g) - 1)
	v := &(*g)[0]
	for true {
		v.index = -2
		for j := 0; j < len(v.edges); j++ {
			a := v.edges[j].len
			u := &(*g)[v.edges[j].B]
			if u.index == -1 {
				u.key = a
				u.value = v
				Insert(&q, u)
			} else if u.index != -2 && v.edges[j].len < u.key {
				u.value = v
				Decreasekey(&q, u, v.edges[j].len)
			}
		}
		if QueueEmpty(&q) {
			break
		}
		v = Extractmin(&q)
		res = append(res, *v)
	}
	return res
}

func main() {
	var n, m int
	fmt.Scan(&n, &m)
	g := make([]Vertex, n)
	for i := 0; i < m; i++ {
		var a, b, len int
		fmt.Scan(&a, &b, &len)
		var e Edge
		e.A = a
		e.B = b
		e.len = len
		g[a].edges = append(g[a].edges, e)
		e.A, e.B = e.B, e.A
		g[b].edges = append(g[b].edges, e)
	}

	res := Prim(&g)
	sum := 0
	for i := 0; i < len(res); i++ {
		sum += res[i].key
	}
	fmt.Println(sum)
}
