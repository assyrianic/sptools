package main

import (
	"fmt"
	"os"
	///"io/ioutil"
	"./sptools"
)


func main() {
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIPCOMMENTS
	///**
	if sp_plugin, res := SPTools.LexFile(os.Args[1], lexing_flags); res {
		tok_output, _ := os.Create("sptools_token_output.txt")
		for _, tk := range sp_plugin {
			fmt.Fprintf(tok_output, "%s\n", tk.ToString())
		}
		fmt.Fprintf(tok_output, "number of tokens: %d\n", len(sp_plugin))
	} else {
		fmt.Printf("failed to process tokens\n")
	}
	//*/
	///**
	if sp_plugin := SPTools.ParseFile(os.Args[1], lexing_flags); sp_plugin != nil {
		node_output, _ := os.Create("sptools_node_output.txt")
		SPTools.PrintNode(sp_plugin, 0, node_output)
	} else {
		fmt.Printf("failed to parse\n")
	}
	//*/
}
