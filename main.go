package main

import (
	"fmt"
	"os"
	//"io/ioutil"
	"./sptools"
)


func main() {
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIPCOMMENTS
	if sp_plugin := SPTools.ParseFile(os.Args[1], lexing_flags); sp_plugin != nil {
		SPTools.PrintNode(sp_plugin, 0)
	} else {
		fmt.Printf("failed to parse\n")
	}
	
	/*
	if sp_plugin, res := SPTools.LexFile(os.Args[1], lexing_flags); res {
		for _, tk := range sp_plugin {
			fmt.Printf("token output: %q - line: %d, col: %d, token type: %d | filename: %s\n", tk.Lexeme, tk.Line, tk.Col, tk.Kind, *tk.Path)
		}
		fmt.Printf("number of tokens: %d\n", len(sp_plugin))
	} else {
		fmt.Printf("failed to process tokens\n")
	}
	 */
}
