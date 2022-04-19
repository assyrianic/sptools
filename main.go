package main

import (
	"fmt"
	"os"
	///"io/ioutil"
	"./sptools"
)


func main() {
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIPCOMMENTS
	///*
	if sp_plugin := SPTools.ParseFile(os.Args[1], lexing_flags); sp_plugin != nil {
		node_output, _ := os.Create("sptools_node_output.txt")
		SPTools.PrintNode(sp_plugin, 0, node_output)
	} else {
		fmt.Printf("failed to parse\n")
	}
	//*/
	/**
	if sp_plugin, res := SPTools.LexFile(os.Args[1], lexing_flags); res {
		node_output, _ := os.Create("sptools_node_output.txt")
		for _, tk := range sp_plugin {
			fmt.Fprintf(node_output, "token output: %q - line: %d, col: %d, token type: %d | filename: %s\n", tk.Lexeme, tk.Line, tk.Col, tk.Kind, *tk.Path)
		}
		fmt.Fprintf(node_output, "number of tokens: %d\n", len(sp_plugin))
	} else {
		fmt.Printf("failed to process tokens\n")
	}
	*/
}
