package main

import "fmt"

func add(a, b []int32, p int) []int32 {
	var lenres, lenmin int

	if len(a) > len(b) {
		lenres = len(a) + 1
		lenmin = len(b)
	} else {
		lenres = len(b) + 1
		lenmin = len(a)
	}

	var res []int32
	res = append(res, 0)
	for i := 0; i < lenmin; i++ {
		res = append(res, 0)
		res[i] += a[i] + b[i]
		for res[i] >= int32(p) {
			res[i] -= int32(p)
			res[i+1] += 1
		}
	}
	res = append(res, 0)
	for i := lenmin; i < lenres-1; i++ {
		if len(a) > len(b) {
			res[i] += a[i]
			for res[i] >= int32(p) {
				res[i] -= int32(p)
				res[i+1] += 1
			}
		} else {
			res[i] += b[i]
			for res[i] >= int32(p) {
				res[i] -= int32(p)
				res[i+1] += 1
			}
		}
	}

	if res[len(res)-1] == 0 {
		res = res[:len(res)-1]
	}
	return res
}

func main() {
	var (
		a = []int32{1, 4, 0, 9, 9}
		b = []int32{4, 7, 9, 7, 8, 9}
		p = 11
	)

	fmt.Print(add(a, b, p)) //Если вывод в виде массива

	/*res := add(a, b, p) //Если вывод в виде числа
	for i, _ := range res {
		fmt.Print(res[len(res)-i-1])
	}*/
}
