package main

import (
	"fmt"
	"os"
	//"io/ioutil"
	"./sptools"
)


func main() {
	lexing_flags := SPTools.LEXFLAG_PREPROCESS | SPTools.LEXFLAG_STRIPNEWLINES
	if sp_plugin := SPTools.ParseFile(os.Args[1], lexing_flags); sp_plugin != nil {
		SPTools.Walk(sp_plugin, func(n SPTools.Node) bool {
			
			//switch ast := n.(type) {
				//default:
					fmt.Printf("%T\n", n)
			//}
			return true
		})
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
