package main

import (
	"bufio"
	"fmt"
	"os"
)

func args(str string) [2]string {
	var (
		op_br = 0
		cl_br = 0
		args  [2]string
	)

	for str[0] == ' ' {
		str = str[1:]
	}

	if str[0] != '(' {
		args[0] = string(str[0])
	} else {
		op_br += 1
		for op_br != cl_br {
			str = str[1:]
			if str[0] == '(' {
				op_br += 1
			}
			if str[0] == ')' {
				cl_br += 1
			}
			if str[0] != ' ' {
				args[0] += string(str[0])
			}
		}
	}
	op_br, cl_br = 0, 0

	str = str[1:]
	for str[0] == ' ' {
		str = str[1:]
	}

	if str[0] != '(' {
		args[1] = string(str[0])
	} else {
		op_br += 1
		for op_br != cl_br {
			str = str[1:]
			if str[0] == '(' {
				op_br += 1
			}
			if str[0] == ')' {
				cl_br += 1
			}
			args[1] += string(str[0])
		}
	}
	return args
}

func SCHEME(str string) int {
	if str[0] == '+' {
		args := args(str[1:])
		return SCHEME(args[0]) + SCHEME(args[1])
	} else if str[0] == '-' {
		args := args(str[1:])
		return SCHEME(args[0]) - SCHEME(args[1])
	} else if str[0] == '*' {
		args := args(str[1:])
		return SCHEME(args[0]) * SCHEME(args[1])
	} else if str[0] == ' ' || str[0] == '(' || str[0] == ')' {
		return SCHEME(str[1:])
	} else {
		return int(str[0]) - 48
	}
}

func main() {
	var str string

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	str = scanner.Text()

	fmt.Print(SCHEME(str))
}
