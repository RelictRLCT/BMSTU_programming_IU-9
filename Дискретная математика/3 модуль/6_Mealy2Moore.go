package main

import (
	"fmt"
	"strconv"
)

type Vertex struct {
	vers   []Edge
	exit   string
	number int
	sost   int
}

type Edge struct {
	A, B int
	sig  string
	exit string
}

type NewVer struct {
	sost int
	exit string
	i    int
	j    int
}

func print_res(g []Vertex) {
	fmt.Println("digraph {")
	fmt.Println("    rankdir = LR")
	for i := 0; i < len(g); i++ {
		fmt.Print("    ", i, " [label = \"(")
		fmt.Print(g[i].number)
		fmt.Print(",")
		fmt.Print(g[i].exit)
		fmt.Println(")\"]")
		leng := len(g[i].vers)
		for j := 0; j < leng; j++ {
			fmt.Print("    ", g[i].vers[j].A)
			fmt.Print(" -> ")
			fmt.Print(g[i].vers[j].B)
			fmt.Print(" [label = \"", g[i].vers[j].sig, "\"]")
			fmt.Println()
		}
	}
	fmt.Print("}")
}

func member(Moore []NewVer, nv NewVer) bool {
	for i := 0; i < len(Moore); i++ {
		if Moore[i].sost == nv.sost && Moore[i].exit == nv.exit {
			return true
		}
	}
	return false
}

func main() {

	var n, k_enter, k_exit int

	fmt.Scan(&k_enter)

	enter_alf := make([]string, k_enter)

	for i := 0; i < k_enter; i++ {
		fmt.Scan(&enter_alf[i])
	}

	fmt.Scan(&k_exit)

	exit_alf := make([]string, k_exit)

	for i := 0; i < k_exit; i++ {
		fmt.Scan(&exit_alf[i])
	}

	fmt.Scan(&n)

	D := make([][]int, n)
	for i := 0; i < n; i++ {
		D[i] = make([]int, k_enter)
	}

	F := make([][]string, n)
	for i := 0; i < n; i++ {
		F[i] = make([]string, k_enter)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < k_enter; j++ {
			fmt.Scan(&D[i][j])
		}
	}

	for i := 0; i < n; i++ {
		for j := 0; j < k_enter; j++ {
			fmt.Scan(&F[i][j])
		}
	}

	Moore := make([]NewVer, 0)
	var nv NewVer
	for i := 0; i < n; i++ {
		for j := 0; j < k_enter; j++ {
			nv = NewVer{D[i][j], F[i][j], i, j}
			if !member(Moore, nv) {
				Moore = append(Moore, nv)
			}
		}
	}

	NewD := make([][]int, len(Moore))
	NewF := make([][]string, len(Moore))
	for i := 0; i < len(Moore); i++ {
		NewF[i] = make([]string, k_enter)
		NewD[i] = make([]int, k_enter)
	}

	for i := 0; i < len(Moore); i++ {
		for j := 0; j < k_enter; j++ {
			perehod := D[Moore[i].sost][j]
			vihod := F[Moore[i].sost][j]
			morev := NewVer{perehod, vihod, 0, 0}
			num := 0
			for iter := 0; iter < len(Moore); iter++ {
				if Moore[iter].sost == morev.sost && Moore[iter].exit == morev.exit {
					num = iter
					break
				}
			}
			NewD[i][j] = num
		}
	}

	for i := 0; i < len(Moore); i++ {
		for j := 0; j < k_enter; j++ {
			NewF[i][j] = Moore[i].exit
		}
	}

	fmt.Println("digraph {")
	fmt.Println("    rankdir = LR")
	for i := 0; i < len(Moore); i++ {
		fmt.Print("    ", i, " [label = \"(")
		fmt.Print(Moore[i].sost)
		fmt.Print(",")
		in, _ := strconv.Atoi(Moore[i].exit)
		fmt.Print(exit_alf[in])
		fmt.Println(")\"]")
	}
	for i := 0; i < len(Moore); i++ {
		for j := 0; j < k_enter; j++ {
			fmt.Print("    ", i)
			fmt.Print(" -> ")
			fmt.Print(NewD[i][j])
			fmt.Print(" [label = \"", enter_alf[j], "\"]")
			fmt.Println()
		}
	}
	fmt.Print("}")

}
