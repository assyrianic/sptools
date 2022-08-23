package main

import (
	"fmt"
	"os"
	"./sptools"
)


func main() {
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIP_COMMENTS
	/*
	if plugin_tokens, res := SPTools.LexFile(os.Args[1], lexing_flags, nil); res {
		tok_output, _ := os.Create("sptools_token_output.txt")
		fmt.Fprintf(tok_output, "number of tokens: %d\n", len(plugin_tokens.Tokens))
		for _, tk := range plugin_tokens.Tokens {
			fmt.Fprintf(tok_output, "%s\n", tk.ToString())
		}
		if plugin_node := SPTools.ParseTokens(plugin_tokens, false); plugin_node != nil {
			node_output, _ := os.Create("sptools_node_output.txt")
			SPTools.PrintNode(plugin_node, 0, node_output)
			ast_output, _ := os.Create("sptools_node_pretty_print.txt")
			fmt.Fprintf(ast_output, "%s\n", SPTools.AstToString(plugin_node))
		} else {
			fmt.Printf("failed to parse\n")
		}
	} else {
		fmt.Printf("failed to process tokens\n")
	}
	*/
	///*
	tr, _ := SPTools.LexCodeString(os.Args[1], lexing_flags, nil)
	parser := SPTools.MakeParser(tr)
	e := parser.Statement()
	fmt.Printf("'%s'\n", SPTools.AstToString(e))
	SPTools.PrintNode(e, 0, os.Stdin)
	interp := SPTools.MakeInterpreter(parser)
	flow := SPTools.ControlFlow(0)
	type_res := interp.EvalStmt(e, &flow)
	fmt.Printf("SPTools Expr Evaluation: '%T' - '%v'\n", type_res, type_res)
	//*/
}