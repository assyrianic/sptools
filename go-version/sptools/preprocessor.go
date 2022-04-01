package SPTools

import (
	"os"
	"fmt"
	"strings"
	"time"
	"strconv"
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
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ( where function-like macro but have '%s'.", e.Lexeme)
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
	return output, len(output) > 0
}


// This is for the #if, #elif, #else conditional parsing.
func boolToInt(b bool) int {
	if b {
		return 1
	}
	return 0
}
func intToBool(i int) bool {
	return i != 0
}

// expr = OrExpr .
func evalCond(tokens []SPToken, i *int, macros map[string]spMacro) int {
	return evalOr(tokens, i, macros)
}

// OrExpr = AndExpr *( '||' AndExpr ) .
func evalOr(tokens []SPToken, i *int, macros map[string]spMacro) int {
	res := evalAnd(tokens, i, macros)
	if res < 0 {
		// TODO: change return result as -1 is valid in preprocessor.
		return -1
	}
	for *i < len(tokens) && tokens[*i].Kind==SPTKOrL {
		*i++
		res = boolToInt(intToBool(res) || intToBool(evalAnd(tokens, i, macros)))
	}
	return res
}
// AndExpr = RelExpr *( '&&' RelExpr ) .
func evalAnd(tokens []SPToken, i *int, macros map[string]spMacro) int {
	res := evalRel(tokens, i, macros)
	if res < 0 {
		return -1
	}
	for *i < len(tokens) && tokens[*i].Kind==SPTKAndL {
		*i++
		res = boolToInt(intToBool(res) && intToBool(evalRel(tokens, i, macros)))
	}
	return res
}
// RelExpr = AddExpr *( ( '==' | '!=' | '>=' | '<=' | '<' | '>' ) AddExpr ) .
func evalRel(tokens []SPToken, i *int, macros map[string]spMacro) int {
	res := evalAdd(tokens, i, macros)
	if res < 0 {
		return -1
	}
	for *i < len(tokens) && (tokens[*i].Kind >= SPTKLess && tokens[*i].Kind <= SPTKEq) {
		k := tokens[*i].Kind
		*i++
		switch k {
			case SPTKLess:
				res = boolToInt(res < evalAdd(tokens, i, macros))
			case SPTKGreater:
				res = boolToInt(res > evalAdd(tokens, i, macros))
			case SPTKGreaterE:
				res = boolToInt(res >= evalAdd(tokens, i, macros))
			case SPTKLessE:
				res = boolToInt(res <= evalAdd(tokens, i, macros))
			case SPTKNotEq:
				res = boolToInt(res != evalAdd(tokens, i, macros))
			case SPTKEq:
				res = boolToInt(res == evalAdd(tokens, i, macros))
		}
	}
	return res
}
// AddExpr = MulExpr *( ( '+' | '-' ) MulExpr ) .
func evalAdd(tokens []SPToken, i *int, macros map[string]spMacro) int {
	res := evalMul(tokens, i, macros)
	if res < 0 {
		return -1
	}
	for *i < len(tokens) && (tokens[*i].Kind==SPTKAdd || tokens[*i].Kind==SPTKSub) {
		k := tokens[*i].Kind
		*i++
		switch k {
			case SPTKAdd:
				res += evalMul(tokens, i, macros)
			case SPTKSub:
				res -= evalMul(tokens, i, macros)
		}
	}
	return res
}
// MulExpr = Term *( ( '*' | '/' | '%' ) Term ) .
func evalMul(tokens []SPToken, i *int, macros map[string]spMacro) int {
	res := evalTerm(tokens, i, macros)
	if res < 0 {
		return -1
	}
	for *i < len(tokens) && (tokens[*i].Kind==SPTKMul || tokens[*i].Kind==SPTKDiv || tokens[*i].Kind==SPTKMod) {
		k := tokens[*i].Kind
		*i++
		switch k {
			case SPTKMul:
				res *= evalTerm(tokens, i, macros)
			case SPTKDiv:
				res /= evalTerm(tokens, i, macros)
			case SPTKMod:
				res %= evalTerm(tokens, i, macros)
		}
	}
	return res
}
// Term = ident | 'defined' ident | integer | '(' Expr ')'.
func evalTerm(tokens []SPToken, i *int, macros map[string]spMacro) int {
	switch t := tokens[*i]; t.Kind {
		case SPTKDefined:
			fmt.Printf("got defined check\n")
			*i++
			if *i >= len(tokens) || tokens[*i].Kind != SPTKIdent {
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "'defined' expected identifier but got '%s'.", t.Lexeme)
				return -1
			}
			
			_, found := macros[tokens[*i].Lexeme]
			*i++
			if found {
				return 1
			} else {
				return 0
			}
		case SPTKIdent:
			time.Sleep(500 * time.Millisecond)
			writeMsg(nil, os.Stdout, *t.Path, "log", COLOR_GREEN, &t.Line, &t.Col, "conditional preprocessing macro: '%s'.", t.Lexeme)
			
			macro, found := macros[t.Lexeme]
			if !found {
				if t.Lexeme == "__LINE__" {
					return t.Line
				}
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "undefined symbol '%s'.", t.Lexeme)
				return -1
			}
			
			saved := *i
			expanded, good := macro.apply(tokens, i)
			if macro.funcLike {
				*i++
			}
			if !good {
				*i = saved
				fmt.Printf("not good\n")
				return -1
			} else {
				fmt.Printf("%+v\n", expanded)
				//fmt.Printf("tokens[:saved][0] :: '%s' - tokens[:saved][0] :: '%s'\n", tokens[:saved][0].Lexeme, tokens[saved:][0].Lexeme)
				//tokens = append(tokens[:saved], append(expanded, tokens[saved:]...)...)
				n := 0
				r := evalCond(expanded, &n, macros)
				fmt.Printf("evaluated macro result: %d\n", r)
				return r
			}
		case SPTKIntLit:
			fmt.Printf("got int lit '%s'\n", t.Lexeme)
			d, _ := strconv.ParseInt(t.Lexeme, 0, 64)
			*i++
			return int(d)
		case SPTKLParen:
			fmt.Printf("got parentheses (expr)\n")
			*i++
			exp := evalCond(tokens, i, macros)
			if *i >= len(tokens) || tokens[*i].Kind != SPTKRParen {
				e := tokens[*i]
				writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ')' got '%s' in conditional preprocessor directive.", e.Lexeme)
				return -1
			} else {
				*i++
			}
			return exp
		default:
			writeMsg(nil, os.Stdout, *tokens[*i].Path, "syntax error", COLOR_RED, &tokens[*i].Line, &tokens[*i].Col, "conditional preprocessing requires a constant, integer expression, got '%s'.", tokens[*i].Lexeme)
			return -1
	}
}


type CondInclCtx int
const (
	IN_THEN = CondInclCtx(iota)
	IN_ELIF
	IN_ELSE
)
type spCondInclStack []CondInclCtx

func (stack spCondInclStack) push(ctx CondInclCtx) {
	stack = append(stack, ctx)
}

func (stack spCondInclStack) pop() bool {
	if l := len(stack); l <= 0 {
		return false
	} else {
		stack = stack[:l-1]
		return true
	}
}


func Preprocess(tokens []SPToken) ([]SPToken, bool) {
	var (
		output []SPToken
		ifStack  spCondInclStack
		skipStk *CondInclCtx
	)
	macros, num_tokens := make(map[string]spMacro), len(tokens)
	sptools_define := SPToken{ Lexeme: "1", Path: tokens[0].Path, Line: -1, Col: -1, Kind: SPTKIntLit }
	macros["__SPTOOLS__"] = spMacro{tokens: []SPToken{sptools_define}, params: nil, funcLike: false}
	
	for i:=0; i < num_tokens; i++ {
		t := tokens[i]
		//time.Sleep(500 * time.Millisecond)
		if t.Kind==SPTKIdent {
			if macro, found := macros[t.Lexeme]; found {
				if toks, res := macro.apply(tokens, &i); res {
					output = append(output, toks...)
				}
			} else {
				output = append(output, t)
			}
			continue
		}
		
		if t.Kind==SPTKHash {
			if i + 1 < num_tokens {
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
						goto preprocessing_done
					case "define":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind != SPTKIdent {
								writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "token '%s' where expected identifier in #define.", t2.Lexeme)
								return tokens, false
							}
							if i + 3 < num_tokens {
								if tokens[i+3].Kind==SPTKLParen {
									// function-like macro.
									if macro, offs := makeFuncMacro(tokens[i+4 : len(tokens)]); offs > 0 {
										macros[t2.Lexeme] = macro
										i += offs + 2
									}
								} else {
									// object-like macro.
									macro, offs := makeObjMacro(tokens[i+3 : len(tokens)])
									macros[t2.Lexeme] = macro
									i += offs + 1
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
					case "if":
						ifStack.push(IN_THEN)
						idx := i + 2
						if eval_res := evalCond(tokens, &idx, macros); eval_res==0 {
							stklen := len(ifStack)
							// TODO: have it skip to elifs and elses
						} else {
							fmt.Printf("eval_res from conditional directive: %d\n", eval_res);
							num_tokens = len(tokens)
							i = idx
							for i < num_tokens {
								if tokens[i].Kind != SPTKHash {
									continue
								}
								i++
								if skip := tokens[i]; skip.Lexeme=="if" || skip.Lexeme=="elif" || skip.Lexeme=="else" || skip.Lexeme=="endif" {
									
								}
							}
						}
					case "endif":
						if res := ifStack.pop(); !res {
							writeMsg(nil, os.Stdout, *directive.Path, "syntax error", COLOR_RED, &directive.Line, &directive.Col, "lone #endif.")
							return tokens, false
						}
						i++
					/* TODO:
					case "elif":
					case "else":
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
