package main

import (
	"fmt"
	"os"
	"./sptools"
)


func main() {
	///*
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIP_COMMENTS
	if plugin_tokens, res := SPTools.LexFile(os.Args[1], lexing_flags, nil); res {
		tok_output, _ := os.Create("sptools_token_output.txt")
		fmt.Fprintf(tok_output, "number of tokens: %d\n", len(plugin_tokens))
		for _, tk := range plugin_tokens {
			fmt.Fprintf(tok_output, "%s\n", tk.ToString())
		}
		if plugin_node := SPTools.ParseTokens(plugin_tokens); plugin_node != nil {
			node_output, _ := os.Create("sptools_node_output.txt")
			SPTools.PrintNode(plugin_node, 0, node_output)
		} else {
			fmt.Printf("failed to parse\n")
		}
	} else {
		fmt.Printf("failed to process tokens\n")
	}
	//*/
	
	/*
	if expr_node := SPTools.ParseExpression(os.Args[1], 0, nil); expr_node != nil {
		fmt.Printf("'%s'\n", SPTools.ExprToString(expr_node))
	}
	*/
	/*
	if stmt_node := SPTools.ParseStatement(os.Args[1], 0, nil); stmt_node != nil {
		fmt.Printf("'%s'\n", SPTools.StmtToString(stmt_node))
	}
	*/
	/*
	if pl := SPTools.ParseFile(os.Args[1], SPTools.LEXFLAG_STRIP_COMMENTS, nil); pl != nil {
		fmt.Printf("'%s'\n", SPTools.PluginToString(pl))
	}
	*/
}