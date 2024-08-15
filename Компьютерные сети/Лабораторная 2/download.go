package main

import (
	"fmt"
	"github.com/mgutz/logxi/v1"
	"golang.org/x/net/html"
	"net/http"
)

func getAttr(node *html.Node, key string) string {
	for _, attr := range node.Attr {
		if attr.Key == key {
			return attr.Val
		}
	}
	return ""
}

func getChildren(node *html.Node) []*html.Node {
	var children []*html.Node
	for c := node.FirstChild; c != nil; c = c.NextSibling {
		children = append(children, c)
	}
	return children
}

func isElem(node *html.Node, tag string) bool {
	return node != nil && node.Type == html.ElementNode && node.Data == tag
}

func isDiv(node *html.Node, class string) bool {
	return isElem(node, "div") && getAttr(node, "class") == class
}

type Item struct {
	Ref, Time, Title string
}

func search(node *html.Node) []*Item {
	if isDiv(node, "main__inner l-col-center") && getAttr(node.Parent, "class") == "main__list js-main-news-list" {
		var items []*Item
		for _, a := range getChildren(node) {
			if getAttr(a, "class") == "main__feed js-main-reload-item " {
				for _, b := range getChildren(a) {
					d := getChildren(b)
					if getAttr(b, "href") != "" && d[1].Data == "span" {
						items = append(items, &Item{
							Ref:   getAttr(b, "href"),
							Title: getChildren(getChildren(d[1])[1])[1].Data,
						})
					}
				}
			}
		}
		return items
		fmt.Println(items)
	}

	for c := node.FirstChild; c != nil; c = c.NextSibling {
		if items := search(c); items != nil {
			return items
		}
	}

	return nil
}

func downloadNews() []*Item {
	log.Info("sending request to rbc.ru")
	if response, err := http.Get("https://www.rbc.ru/"); err != nil {
		log.Error("request to rbc.ru failed", "error", err)
	} else {
		defer response.Body.Close()
		status := response.StatusCode
		log.Info("got response from rbc.ru", "status", status)
		if status == http.StatusOK {
			if doc, err := html.Parse(response.Body); err != nil {
				log.Error("invalid HTML from rbc.ru", "error", err)
			} else {
				log.Info("HTML from rbc.ru parsed successfully")
				return search(doc)
			}
		}
	}
	return nil
}
