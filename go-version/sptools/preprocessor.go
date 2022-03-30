package SPTools

import (
	"os"
	"fmt"
	"strings"
	"time"
)


type spMacro struct {
	params    map[string]int
	tokens  []SPToken
	funcLike  bool
}

func makeFuncMacro(tokens []SPToken) (spMacro, int) {
	macro := spMacro{tokens: make([]SPToken, 0), params: make(map[string]int), funcLike: true}
	i, num_tokens := 0, len(tokens)
	for ; i < num_tokens && tokens[i].Kind != SPTKRParen; i++ {
		t := tokens[i]
		if len(macro.params) > 0 {
			if t.Kind==SPTKComma {
				// skip past ','
				i++
				t = tokens[i]
			} else {
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "expected ',' but got '%s' in #define macro.", t.Lexeme)
				return macro, 0
			}
		}
		
		if t.Kind==SPTKMod && i + 1 < num_tokens && tokens[i+1].Kind==SPTKIntLit {
			i++
			macro.params["%" + tokens[i].Lexeme] = len(macro.params)
		} else {
			writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "unexpected param '%s' in #define macro. Params must be %%<integer literal> (i.e. %%1, %%2).", t.Lexeme)
			return macro, 0
		}
	}
	i++
	for ; i < num_tokens && tokens[i].Kind != SPTKNewline; i++ {
		macro.tokens = append(macro.tokens, tokens[i])
	}
	i++
	return macro, i
}

func makeObjMacro(tokens []SPToken) (spMacro, int) {
	macro := spMacro{tokens: make([]SPToken, 0), params: nil, funcLike: false}
	i := 0
	for ; i < len(tokens) && tokens[i].Kind != SPTKNewline; i++ {
		macro.tokens = append(macro.tokens, tokens[i])
	}
	i++
	return macro, i
}

func (macro spMacro) apply(tokens []SPToken, i *int) ([]SPToken, bool) {
	num_tokens := len(tokens)
	var output []SPToken
	if macro.funcLike {
		if *i+1 < num_tokens && tokens[*i+1].Kind != SPTKLParen {
			e := tokens[*i+1]
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ( where function-like macro: '%s'.", e.Lexeme)
			return tokens, false
		}
		args := make(map[int]SPToken)
		idx, num_arg := *i + 2, 0
		for idx < num_tokens && tokens[idx].Kind != SPTKRParen {
			if tokens[idx].Kind==SPTKComma {
				idx++
			}
			args[num_arg] = tokens[idx]
			num_arg++
			idx++
		}
		if len(macro.params) != len(args) {
			e := tokens[*i+1]
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "function macro args given (%d) do not match parameters (%d).", len(args), len(macro.params))
			return tokens, false
		}
		*i = idx
		for n:=0; n < len(macro.tokens); n++ {
			x := macro.tokens[n]
			if x.Kind==SPTKIdent && x.Lexeme=="__LINE__" {
				line_str := fmt.Sprintf("%d", x.Line)
				output = append(output, SPToken{Lexeme: line_str, Path: x.Path, Line: x.Line, Col: x.Col, Kind: SPTKIntLit})
			} else if x.Kind==SPTKMod && n + 1 < len(macro.tokens) && macro.tokens[n+1].Kind==SPTKIntLit {
				if param, found := macro.params["%" + macro.tokens[n+1].Lexeme]; found {
					output = append(output, args[param])
				}
				n++
			} else if x.Kind==SPTKHash && n + 1 < len(macro.tokens) && macro.tokens[n+1].Kind==SPTKMod && n + 2 < len(macro.tokens) && macro.tokens[n+2].Kind==SPTKIntLit {
				// stringify
				if param, found := macro.params["%" + macro.tokens[n+2].Lexeme]; found {
					stringified := args[param]
					stringified.Kind = SPTKStrLit
					output = append(output, stringified)
				}
				n += 2
			} else {
				output = append(output, x)
			}
		}
	} else {
		// object-like macro.
		for _, x := range macro.tokens {
			if x.Kind==SPTKIdent && x.Lexeme=="__LINE__" {
				line_str := fmt.Sprintf("%d", x.Line)
				output = append(output, SPToken{Lexeme: line_str, Path: x.Path, Line: x.Line, Col: x.Col, Kind: SPTKIntLit})
			} else {
				output = append(output, x)
			}
		}
	}
	return output, true
}


type CondInclCtx int
const (
	IN_THEN = CondInclCtx(iota + 1)
	IN_ELIF
	IN_ELSE
)

type (
	spCondIncl struct {
		file   *string
		ctx     CondInclCtx
		is_true bool
	}
	spCondIclStack []spCondIncl
)

func (stack *spCondIclStack) readIf(is_true bool) {
	*stack = append(*stack, spCondIncl{file: nil, ctx: IN_THEN, is_true: is_true})
	if !is_true {
		stack.skipCondInc()
	}
}




func Preprocess(tokens []SPToken) ([]SPToken, bool) {
	var output []SPToken
	macros, num_tokens := make(map[string]spMacro), len(tokens)
	macros["__SPTOOLS_COMPILER__"] = spMacro{tokens: make([]SPToken, 0), params: nil, funcLike: false}
	for i:=0; i < num_tokens; i++ {
		t := tokens[i]
		time.Sleep(500 * time.Millisecond)
		//fmt.Printf("preprocess token: %q | %d\n", t.Lexeme, i)
		if t.Kind==SPTKIdent {
			//fmt.Printf("Got Ident %q\n", t.Lexeme)
			if macro, found := macros[t.Lexeme]; found {
				//fmt.Printf("Found existing macro %q!\n", t.Lexeme)
				if toks, res := macro.apply(tokens, &i); res {
					//fmt.Printf("applied macro\n")
					output = append(output, toks...)
				}
			} else {
				//fmt.Printf("didn't find macro\n")
				output = append(output, t)
			}
			continue
		}
		
		if t.Kind==SPTKHash {
			if i + 1 < num_tokens {
				//fmt.Printf("got preproc directive %q!\n", tokens[i+1].Lexeme)
				switch directive := tokens[i+1]; directive.Lexeme {
					case "line", "pragma":
						// skipping this for now.
						idx := i + 1
						for idx < num_tokens && tokens[idx].Kind != SPTKNewline {
							idx++
						}
						idx++
						i = idx
					case "endinput", "endscript":
						//fmt.Printf("ending input\n")
						goto preprocessing_done
					case "define":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind != SPTKIdent {
								writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "token '%s' where expected identifier in #define.", t2.Lexeme)
								return tokens, false
							}
							//fmt.Printf("got named Define: %q\n", t2.Lexeme)
							if i + 3 < num_tokens {
								if tokens[i+3].Kind==SPTKLParen {
									// function-like macro.
									if macro, offs := makeFuncMacro(tokens[i+4 : len(tokens)]); offs > 0 {
										//fmt.Printf("added func-like macro: %q\n", t2.Lexeme)
										macros[t2.Lexeme] = macro
										i += offs + 2
										//fmt.Printf("i is now %d\n", i)
									}
								} else {
									// object-like macro.
									//fmt.Printf("added obj-like macro: %q\n", t2.Lexeme)
									macro, offs := makeObjMacro(tokens[i+3 : len(tokens)])
									macros[t2.Lexeme] = macro
									i += offs + 1
									//fmt.Printf("i is now %d\n", i)
								}
							}
						}
					case "include":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind==SPTKStrLit {
								// local file
								if file_tokens, res := LexFile(t2.Lexeme, true, true); res {
									output = append(output, file_tokens[:len(file_tokens)-1]...)
								} else {
									writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "couldn't find file '%s'.", t2.Lexeme)
									return tokens, false
								}
								i += 2
							} else if t2.Kind==SPTKLess {
								var str_path strings.Builder
								str_path.WriteString("include/")
								idx := i + 3
								for idx < num_tokens && tokens[idx].Kind != SPTKGreater {
									str_path.WriteString(tokens[idx].Lexeme)
									idx++
								}
								str_path.WriteString(".inc")
								idx++
								i = idx
								inc_file := str_path.String()
								if file_tokens, res := LexFile(inc_file, true, true); res {
									output = append(output, file_tokens[:len(file_tokens)-1]...)
								} else {
									writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "couldn't find file '%s'.", inc_file)
									return tokens, false
								}
							}
						}
					case "tryinclude":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind==SPTKStrLit {
								// local file
								if file_tokens, res := LexFile(t2.Lexeme, true, true); res {
									output = append(output, file_tokens[:len(file_tokens)-1]...)
								}
								i += 2
							} else if t2.Kind==SPTKLess {
								// strings.Builder is more efficient to use than the standard string type.
								var str_path strings.Builder
								str_path.WriteString("include/")
								idx := i + 3
								for idx < num_tokens && tokens[idx].Kind != SPTKGreater {
									str_path.WriteString(tokens[idx].Lexeme)
									idx++
								}
								str_path.WriteString(".inc")
								idx++
								i = idx
								inc_file := str_path.String()
								if file_tokens, res := LexFile(inc_file, true, true); res {
									output = append(output, file_tokens[:len(file_tokens)-1]...)
								}
							}
						}
					case "undef":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind != SPTKIdent {
								writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "invalid '%s' name to undef.", t2.Lexeme)
								return tokens, false
							} else {
								delete(macros, t2.Lexeme)
								i += 2
							}
						}
					case "error":
						if i + 2 < num_tokens {
							idx := i + 2
							var error_msg strings.Builder
							for idx < num_tokens && tokens[idx].Kind != SPTKNewline {
								error_msg.WriteString(tokens[idx].Lexeme + " ")
								idx++
							}
							writeMsg(nil, os.Stdout, *tokens[i+2].Path, "user error", COLOR_RED, &tokens[i+2].Line, &tokens[i+2].Col, "%s.", error_msg.String())
							return tokens, false
						}
					case "warning":
						if i + 2 < num_tokens {
							idx := i + 2
							var warn_msg strings.Builder
							for idx < num_tokens && tokens[idx].Kind != SPTKNewline {
								warn_msg.WriteString(tokens[idx].Lexeme + " ")
								idx++
							}
							idx++
							i = idx
							writeMsg(nil, os.Stdout, *tokens[i+2].Path, "user warning", COLOR_MAGENTA, &tokens[i+2].Line, &tokens[i+2].Col, "%s.", warn_msg.String())
						}
					/* TODO:
					case "if":
					case "elif":
					case "else":
					case "endif":
					*/
					default:
						writeMsg(nil, os.Stdout, *directive.Path, "syntax error", COLOR_RED, &directive.Line, &directive.Col, "unknown preprocessor directive: '%s'.", directive.Lexeme)
						return tokens, false
				}
			}
		} else {
			output = append(output, t)
		}
	}
preprocessing_done:
	return output, true
}
