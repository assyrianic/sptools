package SPTools

import (
	"os"
	"fmt"
	"strings"
	///"time"
	"strconv"
)


var (
	preproc_one  = Token{ Lexeme: "1", Path: nil, Line: 0, Col: 0, Kind: TKIntLit }
	preproc_zero = Token{ Lexeme: "0", Path: nil, Line: 0, Col: 0, Kind: TKIntLit }
)


type macro struct {
	params    map[string]int
	tokens  []Token
	funcLike  bool
}

func makeFuncMacro(tokens []Token) (macro, int) {
	macro := macro{tokens: make([]Token, 0), params: make(map[string]int), funcLike: true}
	i, num_tokens := 0, len(tokens)
	for ; i < num_tokens && tokens[i].Kind != TKRParen; i++ {
		t := tokens[i]
		if len(macro.params) > 0 {
			if t.Kind==TKComma {
				// skip past ','
				i++
				t = tokens[i]
			} else {
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "expected ',' but got '%s' in #define macro.", t.Lexeme)
				return macro, 0
			}
		}
		
		if t.Kind==TKMod && i + 1 < num_tokens && tokens[i+1].Kind==TKIntLit {
			i++
			macro.params["%" + tokens[i].Lexeme] = len(macro.params)
		} else {
			writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "unexpected param '%s' in #define macro. Params must be %%<integer literal> (i.e. %%1, %%2).", t.Lexeme)
			return macro, 0
		}
	}
	i++
	for ; i < num_tokens && tokens[i].Kind != TKNewline; i++ {
		macro.tokens = append(macro.tokens, tokens[i])
	}
	i++
	return macro, i
}

func makeObjMacro(tokens []Token) (macro, int) {
	macro := macro{tokens: make([]Token, 0), params: nil, funcLike: false}
	i := 0
	for ; i < len(tokens) && tokens[i].Kind != TKNewline; i++ {
		macro.tokens = append(macro.tokens, tokens[i])
	}
	i++
	return macro, i
}

func (macro macro) apply(tokens []Token, i *int) ([]Token, bool) {
	num_tokens := len(tokens)
	var output []Token
	if macro.funcLike {
		if *i+1 < num_tokens && tokens[*i+1].Kind != TKLParen {
			e := tokens[*i+1]
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ( where function-like macro but have '%s'.", e.Lexeme)
			return tokens, false
		}
		args := make(map[int]Token)
		idx, num_arg := *i + 2, 0
		for idx < num_tokens && tokens[idx].Kind != TKRParen {
			if tokens[idx].Kind==TKComma {
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
			if x.Kind==TKIdent && x.Lexeme=="__LINE__" {
				line_str := fmt.Sprintf("%d", x.Line)
				output = append(output, Token{Lexeme: line_str, Path: x.Path, Line: x.Line, Col: x.Col, Kind: TKIntLit})
			} else if x.Kind==TKMod && n + 1 < len(macro.tokens) && macro.tokens[n+1].Kind==TKIntLit {
				if param, found := macro.params["%" + macro.tokens[n+1].Lexeme]; found {
					output = append(output, args[param])
				}
				n++
			} else if x.Kind==TKHash && n + 1 < len(macro.tokens) && macro.tokens[n+1].Kind==TKMod && n + 2 < len(macro.tokens) && macro.tokens[n+2].Kind==TKIntLit {
				// stringify
				if param, found := macro.params["%" + macro.tokens[n+2].Lexeme]; found {
					stringified := args[param]
					stringified.Kind = TKStrLit
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
			if x.Kind==TKIdent && x.Lexeme=="__LINE__" {
				line_str := fmt.Sprintf("%d", x.Line)
				output = append(output, Token{Lexeme: line_str, Path: x.Path, Line: x.Line, Col: x.Col, Kind: TKIntLit})
			} else {
				output = append(output, x)
			}
		}
	}
	return output, len(output) > 0
}


// This is for the #if, #elseif, #else conditional parsing.
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
func evalCond(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	return evalOr(tokens, i, macros)
}

// OrExpr = AndExpr *( '||' AndExpr ) .
func evalOr(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	sum, res := evalAnd(tokens, i, macros)
	if !res {
		return 0, false
	}
	for *i < len(tokens) && tokens[*i].Kind==TKOrL {
		*i++
		s, b := evalAnd(tokens, i, macros)
		if !b {
			return 0, false
		}
		sum = boolToInt(intToBool(sum) || intToBool(s))
	}
	return sum, true
}
// AndExpr = RelExpr *( '&&' RelExpr ) .
func evalAnd(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	sum, res := evalRel(tokens, i, macros)
	if !res {
		return 0, false
	}
	for *i < len(tokens) && tokens[*i].Kind==TKAndL {
		*i++
		s, b := evalRel(tokens, i, macros)
		if !b {
			return 0, false
		}
		sum = boolToInt(intToBool(sum) && intToBool(s))
	}
	return sum, true
}
// RelExpr = AddExpr *( ( '==' | '!=' | '>=' | '<=' | '<' | '>' ) AddExpr ) .
func evalRel(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	sum, res := evalAdd(tokens, i, macros)
	if !res {
		return 0, false
	}
	for *i < len(tokens) && (tokens[*i].Kind >= TKLess && tokens[*i].Kind <= TKEq) {
		k := tokens[*i].Kind
		*i++
		s, b := evalAdd(tokens, i, macros)
		if !b {
			return 0, false
		}
		switch k {
			case TKLess:
				sum = boolToInt(sum < s)
			case TKGreater:
				sum = boolToInt(sum > s)
			case TKGreaterE:
				sum = boolToInt(sum >= s)
			case TKLessE:
				sum = boolToInt(sum <= s)
			case TKNotEq:
				sum = boolToInt(sum != s)
			case TKEq:
				sum = boolToInt(sum == s)
		}
	}
	return sum, true
}
// AddExpr = MulExpr *( ( '+' | '-' ) MulExpr ) .
func evalAdd(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	sum, res := evalMul(tokens, i, macros)
	if !res {
		return 0, false
	}
	for *i < len(tokens) && (tokens[*i].Kind==TKAdd || tokens[*i].Kind==TKSub) {
		k := tokens[*i].Kind
		*i++
		s, b := evalMul(tokens, i, macros)
		if !b {
			return 0, false
		}
		switch k {
			case TKAdd:
				sum += s
			case TKSub:
				sum -= s
		}
	}
	return sum, true
}
// MulExpr = Prefix *( ( '*' | '/' | '%' ) Prefix ) .
func evalMul(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	sum, res := evalPrefix(tokens, i, macros)
	if !res {
		return 0, false
	}
	for *i < len(tokens) && (tokens[*i].Kind==TKMul || tokens[*i].Kind==TKDiv || tokens[*i].Kind==TKMod) {
		k := tokens[*i].Kind
		*i++
		s, b := evalPrefix(tokens, i, macros)
		if !b {
			return 0, false
		}
		switch k {
			case TKMul:
				sum *= s
			case TKDiv:
				sum /= s
			case TKMod:
				sum %= s
		}
	}
	return sum, true
}

// Prefix = *( '!' | '~' | '-' ) Term
func evalPrefix(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	switch t := tokens[*i]; t.Kind {
		case TKNot:
			*i++
			val, res := evalPrefix(tokens, i, macros)
			return boolToInt(!intToBool(val)), res
		case TKCompl:
			*i++
			val, res := evalPrefix(tokens, i, macros)
			return ^val, res
		case TKSub:
			*i++
			val, res := evalPrefix(tokens, i, macros)
			return -val, res
		default:
			return evalTerm(tokens, i, macros)
	}
}

// Term = ident | 'defined' ident | integer | '(' Expr ')'.
func evalTerm(tokens []Token, i *int, macros map[string]macro) (int, bool) {
	switch t := tokens[*i]; t.Kind {
		case TKDefined:
			///fmt.Printf("got defined check\n")
			*i++
			if *i >= len(tokens) || tokens[*i].Kind != TKIdent {
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "'defined' expected identifier but got '%s'.", t.Lexeme)
				return 0, false
			}
			
			_, found := macros[tokens[*i].Lexeme]
			*i++
			if found {
				return 1, true
			} else {
				return 0, true
			}
		case TKIdent:
			///time.Sleep(100 * time.Millisecond)
			///writeMsg(nil, os.Stdout, *t.Path, "log", COLOR_GREEN, &t.Line, &t.Col, "conditional preprocessing macro: '%s'.", t.Lexeme)
			
			macro, found := macros[t.Lexeme]
			if !found {
				if t.Lexeme == "__LINE__" {
					return int(t.Line), true
				}
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "undefined symbol '%s'.", t.Lexeme)
				return 0, false
			}
			
			// if a macro has no tokens at all, assign it a zero token.
			if len(macro.tokens) <= 0 {
				macro.tokens = []Token{preproc_zero}
			}
			
			saved := *i
			expanded, good := macro.apply(tokens, i)
			
			// skips past ( or ending ) if macro function or skip name if macro object.
			*i++
			
			if !good {
				*i = saved
				///fmt.Printf("not good\n")
				return 0, false
			} else {
				///fmt.Printf("expanded: %v - %v\n", expanded, tokens[*i])
				n := 0
				r, b := evalCond(expanded, &n, macros)
				///fmt.Printf("evaluated macro result: %d\n", r)
				return r, b
			}
		case TKIntLit:
			///fmt.Printf("got int lit '%s'\n", t.Lexeme)
			d, _ := strconv.ParseInt(t.Lexeme, 0, 64)
			*i++
			return int(d), true
		case TKLParen:
			///fmt.Printf("got parentheses (expr)\n")
			*i++
			exp, success := evalCond(tokens, i, macros)
			if *i >= len(tokens) || tokens[*i].Kind != TKRParen {
				e := tokens[*i]
				writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ')' got '%s' in conditional preprocessor directive.", e.Lexeme)
				return 0, false
			} else if !success {
				e := tokens[*i]
				writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "parsing conditional preprocessor expression failed.")
				return 0, false
			} else {
				*i++
			}
			return exp, success
		default:
			writeMsg(nil, os.Stdout, *tokens[*i].Path, "syntax error", COLOR_RED, &tokens[*i].Line, &tokens[*i].Col, "conditional preprocessing requires a constant, integer expression, got '%s'.", tokens[*i].Lexeme)
			return 0, false
	}
}


type (
	CondInclCtx       int
	CondInclStack []CondInclCtx
)
const (
	IN_THEN = CondInclCtx(iota)
	IN_ELIF
	IN_ELSE
)

func (stack *CondInclStack) push(ctx CondInclCtx) {
	*stack = append(*stack, ctx)
}

func (stack *CondInclStack) peek() CondInclCtx {
	if len(*stack) <= 0 {
		return CondInclCtx(-1)
	}
	return (*stack)[len(*stack) - 1]
}

func (stack *CondInclStack) pop() bool {
	if l := len(*stack); l <= 0 {
		return false
	} else {
		*stack = (*stack)[:l-1]
		return true
	}
}


func skipToNextLine(tokens []Token, i *int) {
	for *i < len(tokens) && tokens[*i].Kind != TKNewline {
		*i++
	}
	// skip past the newline as well.
	if *i < len(tokens) {
		*i++
	}
}

/* 
 * scenario where we have:
 * #if
 *     code1
 * #else
 *     code2
 * #endif
 * 
 * when #if is true, we get all the tokens that's between
 * the #if and the #else.
 * 
 * After that, the #else has to be skipped over
 */
func skipToNextCondInclDirective(tokens []Token, i *int) {
	for *i < len(tokens) && tokens[*i].Kind != TKHash {
		skipToNextLine(tokens, i)
	}
}

func tokenizeBetweenCondInclDirective(tokens []Token, i *int) []Token {
	saved := *i
	goToNextCondIncl(tokens, i)
	return tokens[saved : *i - 1]
}

// if the #if fails, we use this.
func goToNextCondIncl(tokens []Token, i *int) bool {
	nest, num_tokens := 0, len(tokens)
	for *i < num_tokens {
		skipToNextCondInclDirective(tokens, i)
		if *i < num_tokens && tokens[*i].Kind==TKHash {
			*i++
			switch cmd := tokens[*i]; cmd.Lexeme {
				case "if":
					///fmt.Printf("skipping over nested if\n")
					nest++
				case "endif":
					if nest > 0 {
						///fmt.Printf("skipping over nested endif\n")
						nest--
					} else if nest==0 {
						///fmt.Printf("went to next endif %v\n", cmd)
						return true
					}
				case "else", "elseif":
					if nest==0 {
						///fmt.Printf("found %v\n", cmd)
						return true
					}
			}
		}
	}
	return false
}


func Preprocess(tokens []Token) ([]Token, bool) {
	macros := make(map[string]macro)
	macros["__SPTOOLS__"] = macro{tokens: []Token{preproc_one}, params: nil, funcLike: false}
	var ifStack CondInclStack
	return preprocess(tokens, ifStack, macros)
}

func preprocess(tokens []Token, ifStack CondInclStack, macros map[string]macro) ([]Token, bool) {
	var output []Token
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		t := tokens[i]
		///time.Sleep(100 * time.Millisecond)
		if t.Kind==TKIdent {
			if macro, found := macros[t.Lexeme]; found {
				if toks, res := macro.apply(tokens, &i); res {
					toks, _ = preprocess(toks, ifStack, macros)
					output = append(output, toks...)
				}
			} else {
				output = append(output, t)
			}
			continue
		}
		
		if t.Kind==TKHash {
			if i + 1 < num_tokens {
				switch directive := tokens[i+1]; directive.Lexeme {
					case "line", "pragma":
						// skipping this for now.
						idx := i + 1
						skipToNextLine(tokens, &idx)
						i = idx
					case "endinput", "endscript":
						goto preprocessing_done
					case "define":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind != TKIdent {
								writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "token '%s' where expected identifier in #define.", t2.Lexeme)
								return tokens, false
							}
							if i + 3 < num_tokens {
								if tokens[i+3].Kind==TKLParen {
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
					case "include", "tryinclude":
						if i + 2 < num_tokens {
							inc_file := ""
							is_optional := directive.Lexeme=="tryinclude"
							t2 := tokens[i+2]
							if t2.Kind==TKStrLit {
								// local file
								inc_file = t2.Lexeme
								i += 2
							} else {
								// strings.Builder is more efficient.
								var str_path strings.Builder
								str_path.WriteString("include/")
								idx := i + 3
								for idx < num_tokens && tokens[idx].Kind != TKGreater {
									str_path.WriteString(tokens[idx].Lexeme)
									idx++
								}
								i = idx + 1
								str_path.WriteString(".inc")
								inc_file = str_path.String()
							}
							
							if filetext, read_err := loadFile(inc_file); filetext != "" {
								inc_tokens := Tokenize(filetext, inc_file)
								inc_tokens  = ConcatStringLiterals(inc_tokens)
								if preprocd, res := preprocess(inc_tokens, ifStack, macros); !res {
									writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "failed to preprocess '%s' -- '%s'.", inc_file, read_err)
									return tokens, false
								} else {
									preprocd = StripNewlineTokens(preprocd)
									output = append(output, preprocd[:len(preprocd)-1]...)
								}
							} else if !is_optional {
								writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "couldn't find file '%s'.", inc_file)
								return tokens, false
							}
						}
					case "undef":
						if i + 2 < num_tokens {
							t2 := tokens[i+2]
							if t2.Kind != TKIdent {
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
							for idx < num_tokens && tokens[idx].Kind != TKNewline {
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
							for idx < num_tokens && tokens[idx].Kind != TKNewline {
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
						if eval_res, success := evalCond(tokens, &idx, macros); !success {
							writeMsg(nil, os.Stdout, *tokens[i+2].Path, "syntax error", COLOR_RED, &tokens[i+2].Line, &tokens[i+2].Col, "parsing conditional preprocessor directive failed.")
							return tokens, false
						} else {
							if eval_res != 0 {
								///fmt.Printf("#if eval_res from conditional directive: %d\n", eval_res)
								skipToNextLine(tokens, &idx)
								betweeners := tokenizeBetweenCondInclDirective(tokens, &idx)
								///fmt.Printf("#if betweeners:: %v\n", betweeners)
								betweeners, _ = preprocess(betweeners, ifStack, macros)
								///fmt.Printf("#if betweeners preprocessed:: %v\n", betweeners)
								output = append(output, betweeners...)
								///fmt.Printf("#if after:: %v\n", tokens[i])
								
								i = idx + 1
								for i < len(tokens) && tokens[i].Lexeme != "endif" {
									goToNextCondIncl(tokens, &i)
								}
								i -= 2
							} else {
								goToNextCondIncl(tokens, &idx)
								i = idx - 2
								///fmt.Printf("#if failed, going to next conditional directive., current: '%v'\n", tokens[i])
							}
						}
					case "endif":
						if res := ifStack.pop(); !res {
							writeMsg(nil, os.Stdout, *directive.Path, "syntax error", COLOR_RED, &directive.Line, &directive.Col, "stray #endif.")
							return tokens, false
						}
						i++
					case "else":
						idx := i + 2
						if len(ifStack) <= 0 {
							writeMsg(nil, os.Stdout, *directive.Path, "syntax error", COLOR_RED, &directive.Line, &directive.Col, "stray #else.", directive.Lexeme)
							return tokens, false
						}
						
						ifStack.pop()
						ifStack.push(IN_ELSE)
						betweeners := tokenizeBetweenCondInclDirective(tokens, &idx)
						betweeners, _ = preprocess(betweeners, ifStack, macros)
						i = idx + 1
						output = append(output, betweeners...)
						i -= 2
					case "elseif":
						if len(ifStack) <= 0 {
							writeMsg(nil, os.Stdout, *directive.Path, "syntax error", COLOR_RED, &directive.Line, &directive.Col, "stray #elseif.")
							return tokens, false
						} else if context := ifStack.peek(); context==IN_ELSE {
							writeMsg(nil, os.Stdout, *directive.Path, "syntax error", COLOR_RED, &directive.Line, &directive.Col, "#elseif after #else.")
							return tokens, false
						}
						
						ifStack.pop()
						ifStack.push(IN_ELIF)
						idx := i + 2
						if eval_res, success := evalCond(tokens, &idx, macros); !success {
							writeMsg(nil, os.Stdout, *tokens[i+2].Path, "syntax error", COLOR_RED, &tokens[i+2].Line, &tokens[i+2].Col, "parsing conditional preprocessor directive failed.")
							return tokens, false
						} else {
							if eval_res != 0 {
								///fmt.Printf("#elseif eval_res from conditional directive: %d\n", eval_res)
								skipToNextLine(tokens, &idx)
								betweeners := tokenizeBetweenCondInclDirective(tokens, &idx)
								betweeners, _ = preprocess(betweeners, ifStack, macros)
								output = append(output, betweeners...)
								
								i = idx + 1
								for i < len(tokens) && tokens[i].Lexeme != "endif" {
									goToNextCondIncl(tokens, &i)
								}
								i -= 2
							} else {
								goToNextCondIncl(tokens, &idx)
								///fmt.Printf("#elseif failed, going to next conditional directive.\n")
								i = idx - 2
								///fmt.Printf("#elseif fail post '%s'.\n", tokens[i].Lexeme)
							}
						}
					case "assert":
						idx := i + 2
						if eval_res, success := evalCond(tokens, &idx, macros); !success {
							writeMsg(nil, os.Stdout, *tokens[i+2].Path, "syntax error", COLOR_RED, &tokens[i+2].Line, &tokens[i+2].Col, "parsing conditional preprocessor directive failed.")
							return tokens, false
						} else if eval_res==0 {
							i += 2
							var preprocExpr strings.Builder
							for i < len(tokens) && tokens[i].Kind != TKNewline {
								preprocExpr.WriteString(tokens[i].Lexeme)
								i++
							}
							writeMsg(nil, os.Stdout, *tokens[i+2].Path, "preprocess error", COLOR_RED, &tokens[i+2].Line, &tokens[i+2].Col, "assertion failed: %s", preprocExpr.String())
							return tokens, false
						} else {
							i = idx + 1
						}
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