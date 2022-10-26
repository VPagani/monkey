package main

import (
	"fmt"
	"monkey/repl"
	"os"
	"os/user"
	"strings"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	username := strings.Split(user.Username, "\\")[1]

	fmt.Printf("\n")
	fmt.Printf("Hello %s! This is the Monkey programming language!\n", username)
	fmt.Printf("Feel free to type in commands\n\n")
	repl.Start(os.Stdin, os.Stdout)
}
