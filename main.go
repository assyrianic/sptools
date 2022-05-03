package main

import (
	"fmt"
	"os"
	"./sptools"
)


func main() {
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIPCOMMENTS
	if plugin_tokens, res := SPTools.LexFile(os.Args[1], lexing_flags); res {
		tok_output, _ := os.Create("sptools_token_output.txt")
		for _, tk := range plugin_tokens {
			fmt.Fprintf(tok_output, "%s\n", tk.ToString())
		}
		fmt.Fprintf(tok_output, "number of tokens: %d\n", len(plugin_tokens))
		///*
		if plugin_node := SPTools.ParseTokens(plugin_tokens); plugin_node != nil {
			node_output, _ := os.Create("sptools_node_output.txt")
			SPTools.PrintNode(plugin_node, 0, node_output)
		} else {
			fmt.Printf("failed to parse\n")
		}
		//*/
	} else {
		fmt.Printf("failed to process tokens\n")
	}
}