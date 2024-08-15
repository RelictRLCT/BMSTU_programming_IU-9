package main

import (
	"bufio"
	"fmt"
	"golang.org/x/crypto/ssh"
	"log"
	"os"
)

func main() {
	sshConfig := &ssh.ClientConfig{
		User: "test",
		Auth: []ssh.AuthMethod{
			ssh.Password("SDHBCXdsedfs222"),
		},
		HostKeyCallback: ssh.InsecureIgnoreHostKey(),
	}

	connection, err := ssh.Dial("tcp", "151.248.113.144:443", sshConfig)
	if err != nil {
		log.Fatalf("Ошибка подключения: %s", err)
		return
	}

	fmt.Println("Подключение к серверу 151.248.113.144:443 по SSH выполнено успешно.")

	var command string
	scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Print("> ")

		if scanner.Scan() {
			command = scanner.Text()

			session, err := connection.NewSession()
			if err != nil {
				fmt.Errorf("Ошибка создания сессии: %s", err)
				return
			}
			session.Stdout = os.Stdout
			session.Stderr = os.Stderr

			if err := session.Run(command); err != nil {
				log.Printf("Ошибка выполнения команды '%s': %s\n", command, err)
			}

			session.Close()
		}
	}

}
