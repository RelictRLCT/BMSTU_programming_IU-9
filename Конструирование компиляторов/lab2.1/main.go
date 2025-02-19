package main

import (
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"os"
)

func makefulltypes(file *ast.File) {
	// Вызываем обход дерева, начиная от корня
	ast.Inspect(file, func(node ast.Node) bool {
		// Для каждого узла дерева
		if funcstmt, ok := node.(*ast.FuncType); ok {
			var newlist []*ast.Field
			for _, param := range funcstmt.Params.List {
				if len(param.Names) > 1 {
					for _, name := range param.Names {
						newparam := &ast.Field{
							Names:   []*ast.Ident{name},
							Type:    param.Type,
							Tag:     param.Tag,
							Comment: param.Comment,
							Doc:     param.Doc,
						}
						newlist = append(newlist, newparam)
					}
				} else {
					newlist = append(newlist, param)
				}
			}
			funcstmt.Params.List = newlist
		}
		// Возвращая true, мы разрешаем выполнять обход
		// дочерних узлов
		return true
	})
}

func main() {
	// Создаём хранилище данных об исходных файлах
	fset := token.NewFileSet()

	file, err := parser.ParseFile(
		fset,                 // данные об исходниках
		"demo/demo.go",       // имя файла с исходником программы
		nil,                  // пусть парсер сам загрузит исходник
		parser.ParseComments, // приказываем сохранять комментарии
	)

	// Вызываем парсер
	if err == nil {
		// Если парсер отработал без ошибок, печатаем дерево
		ast.Fprint(os.Stdout, fset, file, nil)
	} else {
		// в противном случае, выводим сообщение об ошибке
		fmt.Printf("Error: %v", err)
	}

	makefulltypes(file)

	f, _ := os.OpenFile("demo/demo_after.go", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	format.Node(f, fset, file)
}
