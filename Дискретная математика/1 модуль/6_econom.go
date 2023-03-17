package main

import (
	"bufio"
	"fmt"
	"os"
)

func member(str string, argument []string) bool {
	for _, x := range argument {
		if str == x {
			return true
		}
	}
	return false
}

func args(str string) [2]string {
	var (
		op_br = 0
		cl_br = 0
		args  [2]string
	)

	if str[0] != '(' {
		args[0] = string(str[0])
	} else {
		op_br += 1
		args[0] += "("
		for op_br != cl_br {
			str = str[1:]
			if str[0] == '(' {
				op_br += 1
			}
			if str[0] == ')' {
				cl_br += 1
			}
			args[0] += string(str[0])
		}
	}
	op_br, cl_br = 0, 0

	str = str[1:]
	if str[0] != '(' {
		args[1] = string(str[0])
	} else {
		op_br += 1
		args[1] += "("
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
	if !member(args[0], arguments) && len(args[0]) > 1 {
		arguments = append(arguments, args[0])
	}
	if !member(args[1], arguments) && len(args[1]) > 1 {
		arguments = append(arguments, args[1])
	}
	return args
}

var arguments []string

func Странный_SCHEME(str string) {
	if str[0] == '#' {
		args := args(str[1:])
		Странный_SCHEME(args[0])
		Странный_SCHEME(args[1])
	} else if str[0] == '$' {
		args := args(str[1:])
		Странный_SCHEME(args[0])
		Странный_SCHEME(args[1])
	} else if str[0] == '@' {
		args := args(str[1:])
		Странный_SCHEME(args[0])
		Странный_SCHEME(args[1])
	} else if str[0] == '(' || str[0] == ')' {
		Странный_SCHEME(str[1:])
	}
}

func main() {
	var str string

	scanner := bufio.NewScanner(os.Stdin) 
	scanner.Scan()                        
	str = scanner.Text()                  

	if str[0] != '(' {
		fmt.Print(0)
		os.Exit(0)
	}
	arguments = append(arguments, str)
	Странный_SCHEME(str)
	fmt.Print(len(arguments))
}
