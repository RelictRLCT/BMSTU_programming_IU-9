package main

import "fmt"

func qsort(n int,
	less func(i, j int) bool,
	swap func(i, j int)) {

	Partition := func(low, high int) int {
		var (
			i, j int
		)
		i = low
		j = low
		for j < high {
			if less(j, high) {
				swap(j, i)
				i += 1
			}
			j += 1
		}
		swap(i, high)
		return i
	}

	var QuickSortRec func(low, high int)

	QuickSortRec = func(low, high int) {
		if low < high {
			var q int
			q = Partition(low, high)
			QuickSortRec(low, q-1)
			QuickSortRec(q+1, high)
		}
	}

	QuickSortRec(0, n-1)

}

func main() {
	var n int
	fmt.Scanf("%d", &n)
	A := make([]int, n)
	for i := 0; i < n; i++ {
		fmt.Scan(&A[i])
	}

	qsort(n, func(i, j int) bool {
		if A[i] < A[j] {
			return true
		} else {
			return false
		}
	}, func(i, j int) { A[i], A[j] = A[j], A[i] })

	for _, x := range A {
		fmt.Printf("%d ", x)
	}
}
