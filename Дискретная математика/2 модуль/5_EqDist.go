package main

import (
	"fmt"
)

type queue struct {
	data  []int
	cap   int
	count int
	head  int
	tail  int
}

func Initqueue(q *queue, n int) {
	q.data = make([]int, n)
	q.cap = n
	q.count = 0
	q.head = 0
	q.tail = 0
}

func queueempty(q *queue) bool {
	return q.count == 0
}

func enqueue(q *queue, x int) {
	q.data[q.tail] = x
	q.tail += 1
	if q.tail == q.cap {
		q.tail = 0
	}
	q.count += 1
}

func dequeue(q *queue) int {
	x := q.data[q.head]
	q.head += 1
	if q.head == q.cap {
		q.head = 0
	}
	q.count -= 1
	return x
}

type ver struct {
	vers []int
	dist []int
}

func BFS(g *[]ver, opor int, visited *[]bool) {
	(*g)[opor].dist = append((*g)[opor].dist, 0)
	var q queue
	dist := 0
	Initqueue(&q, len(*g))
	enqueue(&q, opor)
	(*visited)[opor] = true
	for !queueempty(&q) {
		v := dequeue(&q)
		dist += 1
		for _, i := range (*g)[v].vers {
			if !(*visited)[i] {
				(*visited)[i] = true
				(*g)[i].dist = append((*g)[i].dist, (*g)[v].dist[len((*g)[v].dist)-1]+1)
				enqueue(&q, i)
			}
		}
	}
}

func main() {
	var n, m, k int
	fmt.Scan(&n, &m)
	graph := make([]ver, n)
	for i := 0; i < m; i++ {
		var a, b int
		fmt.Scan(&a, &b)
		graph[a].vers = append(graph[a].vers, b)
		graph[b].vers = append(graph[b].vers, a)
	}
	fmt.Scan(&k)
	for i := 0; i < k; i++ {
		visited := make([]bool, n)
		for j := 0; j < n; j++ {
			visited[j] = false
		}
		var v int
		fmt.Scan(&v)
		BFS(&graph, v, &visited)
	}

	res := make([]int, 0)

	for i := 0; i < len(graph); i++ {
		if len(graph[i].dist) != k {
			continue
		}
		for j := 1; j < len(graph[i].dist); j++ {
			if graph[i].dist[j] != graph[i].dist[j-1] {
				goto A
			}
		}
		res = append(res, i)
	A:
	}
	for i := 0; i < len(res); i++ {
		fmt.Print(res[i], " ")
	}
	if len(res) == 0 {
		fmt.Print("-")
	}
}
