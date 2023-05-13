package main

import (
	"fmt"
	"math"
)

type queue struct {
	heap       []*Vertex
	cap, count int
}

type Vertex struct {
	vers             []*Vertex
	index, dist, sum int
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

func Insert(q *queue, g *[]Vertex, j int) {
	i := q.count
	q.count = i + 1
	q.heap[i] = &(*g)[j]
	for i > 0 && q.heap[(i-1)/2].dist > q.heap[i].dist {
		q.heap[(i-1)/2], q.heap[i] = q.heap[i], q.heap[(i-1)/2]
		q.heap[i].index = i
		i = (i - 1) / 2
	}
	q.heap[i].index = i
}

func Decreasekey(q *queue, v *Vertex, k int) {
	i := v.index
	v.sum = k
	for i > 0 && q.heap[(i-1)/2].dist > k {
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
		if l < n && (*P)[i].dist > (*P)[l].dist {
			i = l
		}
		if r < n && (*P)[i].dist > (*P)[r].dist {
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

func Relax(v *Vertex, u *Vertex) bool {
	changed := (v.dist+u.sum < u.dist)
	if changed {
		u.dist = v.dist + u.sum
	}
	return changed
}

func Dejkstra(g *[]Vertex, s *Vertex) {
	q := Initpqueue(len(*g))
	for i, _ := range *g {
		Insert(&q, g, i)
	}
	for !QueueEmpty(&q) {
		v := Extractmin(&q)
		v.index = -1
		for _, u := range v.vers {
			if u.index != -1 && Relax(v, u) {
				Decreasekey(&q, u, u.dist)
			}
		}
	}
}

func main() {
	var n int
	fmt.Scan(&n)
	g := make([]Vertex, n*n)
	for i := 0; i < n*n; i++ {
		var value int
		fmt.Scan(&value)
		g[i].sum = value
	}
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			g[i*n+j].vers = make([]*Vertex, 0)
			if j != n-1 {
				g[i*n+j].vers = append(g[i*n+j].vers, &g[i*n+j+1])
				g[i*n+j+1].vers = append(g[i*n+j+1].vers, &g[i*n+j])
			}
			if i != n-1 {
				g[i*n+j].vers = append(g[i*n+j].vers, &g[(i+1)*n+j])
				g[(i+1)*n+j].vers = append(g[(i+1)*n+j].vers, &g[i*n+j])
			}
			if i != 0 {
				g[i*n+j].vers = append(g[i*n+j].vers, &g[(i-1)*n+j])
				g[(i-1)*n+j].vers = append(g[(i-1)*n+j].vers, &g[i*n+j])
			}
			if j != 0 {
				g[i*n+j].vers = append(g[i*n+j].vers, &g[i*n+j-1])
				g[i*n+j-1].vers = append(g[i*n+j-1].vers, &g[i*n+j])
			}
		}
	}

	g[0].dist = g[0].sum
	for i := 1; i < n*n; i++ {
		g[i].dist = math.MaxInt
	}
	Dejkstra(&g, &g[0])

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			fmt.Print(g[i*n+j].dist, "   ")
		}
		fmt.Println()
	}

	fmt.Println(g[n*n-1].dist)
}
