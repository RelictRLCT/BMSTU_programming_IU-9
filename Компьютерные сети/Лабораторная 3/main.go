package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

const N = 3

var peers_ports = [N]int{8080, 8081, 8082}

type Message struct {
	Sender string `json:"sender"`
	Text   string `json:"text"`
}

type Client struct {
	port        int
	other_peers []int
	addrs       map[string]bool
	cons        map[net.Conn]bool
	mutex       sync.Mutex
}

func sendMessage(conn net.Conn, message Message) int {
	jsonMessage, err := json.Marshal(message)
	if err != nil {
		fmt.Println("Ошибка кодирования сообщения:", err)
		return 0
	}

	_, err = conn.Write(append(jsonMessage, '\n'))
	if err != nil {
		fmt.Println("Ошибка отправки сообщения:", err)
		return 1
	}
	return 0
}

func listenForMessages(conn net.Conn, client *Client) {
	reader := bufio.NewReader(conn)

	for {
		messageStr, err := reader.ReadString('\n')
		if err != nil {
			return
		}

		var receivedMessage Message
		err = json.Unmarshal([]byte(messageStr), &receivedMessage)
		if err != nil {
			fmt.Println("Ошибка раскодирования сообщения:", err)
			continue
		}

		fmt.Printf("[%s] Полученное сообщение: %s\n", receivedMessage.Sender, receivedMessage.Text)

		words := strings.Split(receivedMessage.Text, "Æ")

		fmt.Println(words)

		if len(receivedMessage.Text) > 15 && receivedMessage.Text[:15] == "NEW Hello from " { //Если отправлено сообщение формата NEW Hello from ...,
			addr := "127.0.0.1:" + receivedMessage.Sender // то добавляется новое подключение
			conn, err := net.Dial("tcp", addr)
			if err != nil {
				fmt.Println("Ошибка соединения с пиром:", err)
				continue
			}

			if client.addrs[addr] == false { //Если к этому адресу еще не подключен
				fmt.Println("Новое подключение ", addr)
				client.mutex.Lock()
				client.cons[conn] = true
				client.addrs[addr] = true
				client.mutex.Unlock()
			}

		} else if len(words) > 2 {
			if ex, _ := exists(words[0]); ex == false {
				f, err := os.Create(words[0])
				if err != nil {
					continue
				}
				for i, str := range words {
					if i == 0 {
						continue
					}
					f.WriteString(str)
					if i != len(words)-1 {
						f.WriteString("\n")
					}
				}
			}
		}
	}
}

func exists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) {
		return false, nil
	}
	return false, err
}

func main() {
	fmt.Println("Введите номер клиента. Максимум ", N-1)
	var n int
	fmt.Scan(&n)
	if n >= N {
		fmt.Println("Введённый номер больше наибольшего числа")
		return
	}

	client := Client{port: peers_ports[n], cons: make(map[net.Conn]bool), addrs: make(map[string]bool)} //инициализация клиента

	for i := 0; i < N; i++ {
		if i != n {
			client.other_peers = append(client.other_peers, peers_ports[i])
		}
	}

	go func() {
		listen, err := net.Listen("tcp", ":"+strconv.Itoa(client.port))
		if err != nil {
			fmt.Println("Ошибка прослушивания:", err)
			os.Exit(1)
		}
		defer listen.Close()

		for {
			conn, err := listen.Accept()
			if err != nil {
				fmt.Println("Ошибка соединения:", err)
				continue
			}

			go listenForMessages(conn, &client)
		}
	}()

	duration := time.Second
	time.Sleep(duration * 5)

	for _, port := range client.other_peers {
		go func(port int) {
			addr := "127.0.0.1:" + strconv.Itoa(port)
			conn, err := net.Dial("tcp", addr)
			if err != nil {
				fmt.Println("Ошибка соединения с пиром:", err)
				return
			}
			if client.addrs[addr] == false {
				client.mutex.Lock()
				client.addrs[addr] = true
				client.cons[conn] = true
				client.mutex.Unlock()
			}

			message := Message{Sender: strconv.Itoa(client.port), Text: "NEW Hello from " + strconv.Itoa(client.port)}
			if sendMessage(conn, message) == 1 {
				client.mutex.Lock()
				client.addrs[conn.RemoteAddr().String()] = false
				client.cons[conn] = false
				client.mutex.Unlock()
			}
		}(port)
	}

	for {
		//открытие файла и чтение данных

		data := make([]string, 0)

		if ex, _ := exists("abc.txt"); ex == true {
			file, err := os.Open("abc.txt")
			if err != nil {
				fmt.Println(err)
				os.Exit(1)
			}

			scanner := bufio.NewScanner(file)

			for scanner.Scan() {
				data = append(data, scanner.Text())
			}

			//fmt.Println("Передаваемый файл", data)
			file.Close()
		}
		//

		buffer := bytes.Buffer{}
		if ex, _ := exists("abc.txt"); ex {
			buffer.WriteString("abc.txt")
			buffer.WriteString("Æ")
			for _, val := range data {
				buffer.WriteString(val)
				buffer.WriteString("Æ")
			}

			text := buffer.String()

			for conn := range client.cons {
				if client.cons[conn] == true {
					message := Message{Sender: strconv.Itoa(client.port), Text: text}
					if sendMessage(conn, message) == 1 {
						client.mutex.Lock()
						client.addrs[conn.RemoteAddr().String()] = false
						client.cons[conn] = false
						client.mutex.Unlock()
					}
					fmt.Println("Отправлено на: ", conn.RemoteAddr().String())
					client.mutex.Lock()
					client.cons[conn] = false                        //
					client.addrs[conn.RemoteAddr().String()] = false //
					client.mutex.Unlock()
				}
			}
		}
	}
}
