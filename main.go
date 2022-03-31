package main

import (
	"fmt"
	"os"
	//"io/ioutil"
	"./sptools"
)

func main() {
	if sp_plugin, res := SPTools.LexFile(os.Args[1], true, true); res {
		for _, tk := range sp_plugin {
			fmt.Printf("token lexeme: %q - line: %d, col: %d, token type: %d | filename: %s\n", tk.Lexeme, tk.Line, tk.Col, tk.Kind, *tk.Path)
		}
		fmt.Printf("number of tokens: %d\n", len(sp_plugin))
	} else {
		fmt.Printf("failed to process tokens\n")
	}
}
