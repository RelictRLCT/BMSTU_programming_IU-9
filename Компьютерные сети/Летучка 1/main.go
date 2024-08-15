package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	MakeRequest()
}

func MakeRequest() {
	resp, err := http.Get("http://pstgu.yss.su/iu9/networks/let1/getkey.php?hash=46304715bc92ea35239ced013f319d2f")
	if err != nil {
		log.Fatalln(err)
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatalln(err)
	}

	body = body[6:]

	otvet := "http://pstgu.yss.su/iu9/networks/let1/send_from_go.php?subject=let1ИУ9-32БНащекинНикита&fio=НащекинНикита&pass="
	strings := []string{otvet, string(body)}
	buffer := bytes.Buffer{}
	for _, val := range strings {
		buffer.WriteString(val)
	}

	fmt.Println(buffer.String())

	resp, err = http.Get(buffer.String())
	if err != nil {
		log.Fatalln(err)
	}

	body, err = ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatalln(err)
	}

	log.Println(string(body))
}
