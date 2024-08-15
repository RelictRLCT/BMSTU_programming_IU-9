package main

import (
	"fmt"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/terminal"
	"io/ioutil"
	"log"
	"net"
	"os/exec"
	"strings"
)

func handleSSHServerConnection(conn net.Conn, config *ssh.ServerConfig) {

	// Handshake for a new SSH connection
	_, chans, _, err := ssh.NewServerConn(conn, config)
	if err != nil {
		log.Fatalf("Failed to handshake (%s)", err)
	}

	log.Printf("SSH server established")

	for newChannel := range chans {
		if newChannel.ChannelType() != "session" {
			newChannel.Reject(ssh.UnknownChannelType, "unknown channel type")
			log.Println("unknown channel type")
			continue
		}

		// Accept the session channel
		channel, _, err := newChannel.Accept()
		if err != nil {
			log.Fatalf("Could not accept channel (%s)", err)
		}

		/*go func() {
			io.Copy(channel, os.Stdout)
			channel.Close()
		}()*/

		go func() {
			term := terminal.NewTerminal(channel, "> ")
			for {
				comm, _ := term.ReadLine()
				fmt.Println(comm)

				words := strings.Fields(comm)

				if len(words) == 1 {
					cmd := exec.Command(words[0])
					cmd.Stdout = channel
					cmd.Stderr = channel
					cmd.Run()
				} else if len(words) != 0 {
					cmd := exec.Command(words[0], words[1:]...)
					cmd.Stdout = channel
					cmd.Stderr = channel
					cmd.Run()
				}
			}
		}()

	}
}

func main() {

	privateKeyBytes, err := ioutil.ReadFile("/home/relict/.ssh/id_rsa")
	if err != nil {
		log.Fatal("Ошибка чтения приватного ключа:", err)
	}

	privateKey, err := ssh.ParsePrivateKey(privateKeyBytes)
	if err != nil {
		log.Fatal("Ошибка разбора приватного ключа:", err)
	}

	config := &ssh.ServerConfig{
		PasswordCallback: func(conn ssh.ConnMetadata, password []byte) (*ssh.Permissions, error) {
			if conn.User() == "test_serv" && string(password) == "relict" {
				return nil, nil
			}
			return nil, fmt.Errorf("Неправильный логин или пароль")
		},
		NoClientAuth: true,
	}

	config.AddHostKey(privateKey)

	listener, err := net.Listen("tcp", "localhost:2222")
	if err != nil {
		log.Fatal("Ошибка создания слушателя:", err)
	}

	log.Printf("SSH-сервер слушает порт %v...", listener.Addr())

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatalf("Ошибка установления соединения: %v", err)
		}
		handleSSHServerConnection(conn, config)
	}

}
