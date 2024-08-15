package main

import (
	"fmt"
	"github.com/mmcdole/gofeed"
	"net/http"
)

func Handler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "<html><body>")
	fmt.Fprintln(w, "<form method=\"POST\">")
	fmt.Fprintln(w, "<label for=\"name\">Переменная:</label>")
	fmt.Fprintln(w, "<input type=\"text\" name=\"val\" id=\"val\"><br>")
	fmt.Fprintln(w, "<input type=\"submit\" value=\"Send\">")
	fmt.Fprintln(w, "</form>")
	fmt.Fprintln(w, "</body></html>")
	message := r.FormValue("val")
	if message == "2" {
		fp := gofeed.NewParser()
		feed, err := fp.ParseURL("https://news.rambler.ru/rss/Voronezh/")
		fmt.Printf("Отправлена страница \"%s\"\n", feed.Title)
		if err == nil {
			fmt.Fprintf(w, "<doctype html>")
			fmt.Fprintf(w, "<h1>%s</h1>\n", feed.Title)
			fmt.Fprintf(w, "<h2>PubDate: %s</h2>\n", feed.Published)
			fmt.Fprintf(w, "<h3>LastBuildDate: %s</h3>\n", feed.Updated)
			fmt.Fprintf(w, "<h2>%s</h2>\n", feed.Description)
			fmt.Fprintf(w, "<br>Number of Items: %d</br>\n", len(feed.Items))
			for v := range feed.Items {
				item := feed.Items[v]
				fmt.Fprintf(w, "<br>Item Number: %d</br>\n", v)
				fmt.Fprintf(w, "<h2>%s</h2>\n", item.Title)
				fmt.Fprintf(w, "<a href=\"%s\">ССЫЛКА НА НОВОСТЬ</a>\n", item.Link)
				fmt.Fprintf(w, "<br>%s</br>\n", item.Description)
				fmt.Fprintf(w, "<br>Guid: %s</br>\n", item.GUID)
			}
		}
	} else if message == "1" {
		fmt.Println("Запущена первая функция")
		fmt.Fprintf(w, message)
		fmt.Fprintf(w, "<img src='1.jpeg' style='width:1500px;height:850px;'>")
	} else if len(message) == 0 {
		fmt.Println("Обновлена страница")
		fmt.Fprintf(w, "Введите запрос")
	} else {
		fmt.Fprintf(w, "Таких функций еще не сущесвтует...")
	}
}

func main() {
	http.Handle("/1.jpeg", http.FileServer(http.Dir("./"))) //Отправка фотки на сервер
	http.HandleFunc("/", Handler)
	http.ListenAndServe(":9000", nil)
}
