package main

import (
	"fmt"
	"os"
	"sort"
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

type Vertex struct {
	vers   []*Vertex
	number int
}

func BFS(g *[]Vertex, start int, visited *[]bool, U, V map[int]struct{}) {
	var q queue
	Initqueue(&q, len(*g))
	enqueue(&q, start)
	(*visited)[start] = true

	U[start] = struct{}{}

	for !queueempty(&q) {
		v := dequeue(&q)
		for _, i := range (*g)[v].vers {
			if !(*visited)[i.number] {
				(*visited)[i.number] = true
				if _, ok := V[v]; ok {
					U[i.number] = struct{}{}
				} else {
					V[i.number] = struct{}{}
				}
				enqueue(&q, i.number)
			}
		}
	}
}

func lexic(first, second []int) bool {
	for i := range first {
		if first[i] < second[i] {
			return true
		} else if first[i] > second[i] {
			return false
		}
	}
	return false
}

func main() {
	var n int
	fmt.Scan(&n)

	D := make([][]string, n)

	for i := 0; i < n; i++ {
		D[i] = make([]string, n)
		for j := 0; j < n; j++ {
			fmt.Scan(&D[i][j])
		}
	}

	g := make([]Vertex, n)

	for i := 0; i < n; i++ {
		g[i].number = i
		for j := 0; j < n; j++ {
			if D[i][j] == "+" {
				g[i].vers = append(g[i].vers, &g[j])
			}
		}
	}

	var bfs_count = 0
	var svobodnie = 0
	svobod := make([]int, 0)

	for i := 0; i < n; i++ {
		if g[i].vers != nil {
			bfs_count += 1
		} else {
			svobodnie += 1
			svobod = append(svobod, i)
		}
	}

	if bfs_count == 0 {
		for i := 0; i < n/2; i++ {
			fmt.Print(i+1, " ")
		}
		os.Exit(0)
	}

	results := make([][][]int, bfs_count)
	for i := 0; i < bfs_count; i++ {
		results[i] = make([][]int, 2)
	}

	var U = make(map[int]struct{})
	var V = make(map[int]struct{})
	visited := make([]bool, n)

	res_count := 0

	for j := 0; j < n; j++ {
		if g[j].vers == nil {
			continue
		}
		for k := range U {
			delete(U, k)
		}
		for k := range V {
			delete(V, k)
		}
		for k := range visited {
			visited[k] = false
		}

		BFS(&g, j, &visited, U, V)
		for i := 0; i < n; i++ {
			if !visited[i] && g[i].vers != nil {
				BFS(&g, i, &visited, U, V)
			}
		}
		for i := range U {
			for _, j := range g[i].vers {
				if _, ok := U[j.number]; ok {
					fmt.Println("No solution")
					os.Exit(0)
				}
			}
		}
		for i := range V {
			for _, j := range g[i].vers {
				if _, ok := V[j.number]; ok {
					fmt.Println("No solution")
					os.Exit(0)
				}
			}
		}

		for i := range U {
			results[res_count][0] = append(results[res_count][0], i)
		}
		for i := range V {
			results[res_count][1] = append(results[res_count][1], i)
		}
		sort.Slice(results[res_count][0], func(i, j int) bool { return results[res_count][0][i] < results[res_count][0][j] })
		sort.Slice(results[res_count][1], func(i, j int) bool { return results[res_count][1][i] < results[res_count][1][j] })
		res_count += 1
	}

	for i := 0; i < bfs_count; i++ {
		var svobod2 = svobod
		for len(svobod) != 0 {
			if len(svobod) != 0 && len(results[i][0]) < len(results[i][1]) {
				results[i][0] = append(results[i][0], svobod[0])
				svobod = svobod[1:]
			} else if len(svobod) != 0 && len(results[i][0]) > len(results[i][1]) {
				results[i][1] = append(results[i][1], svobod[0])
				svobod = svobod[1:]
			} else if len(svobod) != 0 && lexic(results[i][0], results[i][1]) { 
				lens := len(svobod)
				for iter := 0; iter < lens/2; iter++ {
					results[i][0] = append(results[i][0], svobod[iter])
					results[i][1] = append(results[i][1], svobod[lens/2+iter])
				}
				svobod = svobod[lens:]
			} else if len(svobod) != 0 { 
				lens := len(svobod)
				for iter := 0; iter < lens/2; iter++ {
					results[i][1] = append(results[i][1], svobod[iter])
					results[i][0] = append(results[i][0], svobod[lens/2+iter])
				}
				svobod = svobod[lens:]
			}
			sort.Slice(results[i][0], func(k, j int) bool { return results[i][0][k] < results[i][0][j] })
			sort.Slice(results[i][1], func(k, j int) bool { return results[i][1][k] < results[i][1][j] })
		}
		svobod = svobod2
	}

	minresults := make([][]int, bfs_count)

	for i := 0; i < bfs_count; i++ {
		if len(results[i][0]) < len(results[i][1]) {
			minresults[i] = results[i][0]
		} else if len(results[i][1]) < len(results[i][0]) {
			minresults[i] = results[i][1]
		} else if lexic(results[i][0], results[i][1]) {
			minresults[i] = results[i][0]
		} else {
			minresults[i] = results[i][1]
		}
	}

	lexmin := make([]int, bfs_count)
	for i := 0; i < bfs_count; i++ {
		lexmin[i] = n
	}

	for i := 0; i < bfs_count; i++ {
		if lexic(minresults[i], lexmin) {
			lexmin = minresults[i]
		}
	}

	for i := 0; i < len(lexmin); i++ {
		fmt.Print(lexmin[i]+1, " ")
	}

}
