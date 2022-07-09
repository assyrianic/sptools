package SPTools

import (
	"os"
	"fmt"
	"strings"
	"strconv"
	"path/filepath"
	///"time"
)


var (
	PreprocOne  = Token{ Lexeme: "1", Path: nil, Line: 0, Col: 0, Kind: TKIntLit }
	PreprocZero = Token{ Lexeme: "0", Path: nil, Line: 0, Col: 0, Kind: TKIntLit }
)

type Macro struct {
	Params   map[string]int
	Body   []Token
	FuncLike bool
}

func MakeFuncMacro(tr *TokenReader) (Macro, bool) {
	m := Macro{Body: make([]Token, 0), Params: make(map[string]int), FuncLike: true}
	for t := tr.Get(0, TOKFLAG_IGNORE_ALL); tr.Idx < tr.Len() && t.Kind != TKRParen && t.Kind != TKEoF; t = tr.Get(0, TOKFLAG_IGNORE_ALL) {
		if len(m.Params) > 0 {
			if t.Kind==TKComma {
				// skip past ','
				tr.Advance(1)
				t = tr.Get(0, TOKFLAG_IGNORE_ALL)
			} else {
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "expected ',' but got '%s' in #define Macro.", t.Lexeme)
				return m, false
			}
		}
		
		if t.Kind==TKMacroArg {
			///fmt.Printf("MakeFuncMacro :: func-like Macro - t.Lexeme Macro: '%s'\n", t.Lexeme)
			m.Params[t.Lexeme] = len(m.Params)
		} else {
			writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "unexpected param '%s' in #define macro. params must be %%<integer literal> (i.e. %%1, %%2).", t.Lexeme)
			return m, false
		}
		tr.Advance(1)
	}
	///fmt.Printf("MakeFuncMacro :: Params - '%v'\n", m.Params)
	tr.Advance(1) // advance past right parentheses.
	for t := tr.Get(0, TOKFLAG_IGNORE_COMMENT); tr.Idx < tr.Len() && t.Kind != TKNewline && t.Kind != TKEoF; t = tr.Get(0, TOKFLAG_IGNORE_COMMENT) {
		m.Body = append(m.Body, t)
		tr.Advance(1)
	}
	///fmt.Printf("func macro body: '%v'\n", m.Body)
	tr.Advance(1) // advance past newline.
	return m, true
}

func MakeObjMacro(tr *TokenReader) Macro {
	m := Macro{Body: make([]Token, 0), Params: nil, FuncLike: false}
	for t := tr.Get(0, TOKFLAG_IGNORE_COMMENT); tr.Idx < tr.Len() && t.Kind != TKEoF && t.Kind != TKNewline; t = tr.Get(0, TOKFLAG_IGNORE_COMMENT) {
		m.Body = append(m.Body, t)
		tr.Advance(1)
	}
	///fmt.Printf("MakeObjMacro :: object macro body: '%v'\n", m.Body)
	tr.Advance(1) // advance past newline.
	return m
}


func (m Macro) Apply(tr *TokenReader) ([]Token, bool) {
	var output []Token
	name := tr.Get(0, TOKFLAG_IGNORE_ALL)
	tr.Advance(1) // advance past the macro name.
	if m.FuncLike {
		if e := tr.Get(0, 0); e.Kind != TKLParen {
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ( where function-like Macro but have '%s'.", e.Lexeme)
			return output, false
		}
		tr.Advance(1) // advance past (.
		args, num_arg := make(map[int][]Token), 0
		tr.SkipTokenKinds(TKSpace, TKTab)
		nested_parens := 0
		for t := tr.Get(0, TOKFLAG_IGNORE_ALL); t.Kind != TKRParen; t = tr.Get(0, TOKFLAG_IGNORE_ALL) {
			for t.Kind != TKComma && t.Kind != TKRParen {
				// PATCH: WhiteFalcon -- Nested parentheses not properly substituted.
				if t.Kind==TKLParen {
					nested_parens++
				}
				args[num_arg] = append(args[num_arg], t)
				tr.Advance(1)
				t = tr.Get(0, TOKFLAG_IGNORE_ALL)
				if (t.Kind==TKRParen || t.Kind==TKComma) && nested_parens > 0 {
					if t.Kind==TKRParen {
						nested_parens--
					}
					args[num_arg] = append(args[num_arg], t)
					tr.Advance(1)
					t = tr.Get(0, TOKFLAG_IGNORE_ALL)
				}
			}
			///fmt.Printf("Apply :: func-like Macro -> arg['%v']=='%v'\n", num_arg, args[num_arg])
			if t.Kind==TKComma {
				tr.Advance(1)
			}
			num_arg++
		}
		if tr.Get(0, TOKFLAG_IGNORE_ALL).Kind==TKRParen {
			tr.Advance(1)
		}
		if len(m.Params) != len(args) {
			e := tr.Get(1, 0)
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "function Macro %q args given (%d) do not match parameters (%d).", name.Lexeme, len(args), len(m.Params))
			return output, false
		}
		
		macro_len := len(m.Body)
		for n := 0; n < macro_len; n++ {
			if x := m.Body[n]; x.Kind==TKIdent && x.Lexeme=="__LINE__" {
				// line substitution.
				line_str := fmt.Sprintf("%d", x.Line)
				output = append(output, Token{Lexeme: line_str, Path: x.Path, Line: x.Line, Col: x.Col, Kind: TKIntLit})
			} else if (x.Kind==TKIdent || x.Kind==TKIntLit) && n+1 < macro_len && m.Body[n+1].Kind==TKMacroArg {
				kind := x.Kind
				macro_arg := m.Body[n+1]
				n++
				// token pasting.
				///fmt.Printf("Apply :: func-like Macro - Pasting Macro: '%s' + '%s'\n", x.Lexeme, macro_arg.Lexeme)
				if param, found := m.Params[macro_arg.Lexeme]; found {
					new_ident := Token{ Path: args[param][0].Path, Line: args[param][0].Line, Col: args[param][0].Col, Kind: kind }
					new_ident.Lexeme += x.Lexeme
					for _, g := range args[param] {
						new_ident.Lexeme += g.Lexeme
					}
					if n+1 < macro_len && (m.Body[n+1].Kind==TKIdent || m.Body[n+1].Kind==TKIntLit) {
						new_ident.Lexeme += m.Body[n+1].Lexeme
						n++
					}
					///fmt.Printf("Apply :: func-like Macro - Pasting new ident: '%s'\n", new_ident.Lexeme)
					output = append(output, new_ident)
				}
			} else if x.Kind==TKMacroArg {
				// token substitution.
				///fmt.Printf("Apply :: func-like Macro - x.Lexeme Macro: '%s'\n", x.Lexeme)
				if param, found := m.Params[x.Lexeme]; found {
					if n+1 < macro_len && (m.Body[n+1].Kind==TKIdent || m.Body[n+1].Kind==TKIntLit) {
						kind := m.Body[n+1].Kind
						new_ident := Token{ Path: args[param][0].Path, Line: args[param][0].Line, Col: args[param][0].Col, Kind: kind }
						for _, g := range args[param] {
							new_ident.Lexeme += g.Lexeme
						}
						new_ident.Lexeme += m.Body[n+1].Lexeme
						n++
						output = append(output, new_ident)
					} else {
						///fmt.Printf("Apply :: replacing func-like Macro - param (%d) of that Macro: '%v'\n", param, args[param])
						output = append(output, args[param]...)
					}
				}
			} else if x.Kind==TKHashTok {
				// token stringification.
				///fmt.Printf("Apply :: func-like Macro - stringification Macro: '%s'\n", x.Lexeme)
				if param, found := m.Params[ x.Lexeme[1:] ]; found {
					stringified := Token{ Path: args[param][0].Path, Line: args[param][0].Line, Col: args[param][0].Col, Kind: TKStrLit }
					for _, tk := range args[param] {
						stringified.Lexeme += tk.Lexeme
					}
					///fmt.Printf("Apply :: stringified token: '%s'\n", stringified.Lexeme)
					output = append(output, stringified)
				}
			} else {
				output = append(output, x)
			}
		}
	} else {
		// object-like Macro.
		for _, x := range m.Body {
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
func evalCond(tr *TokenReader, macros map[string]Macro) (int, bool) {
	return evalOr(tr, macros)
}

// OrExpr = AndExpr *( '||' AndExpr ) .
func evalOr(tr *TokenReader, macros map[string]Macro) (int, bool) {
	sum, res := evalAnd(tr, macros)
	if !res {
		return 0, false
	}
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	for tr.Get(0, ignore_flags).Kind==TKOrL {
		tr.Advance(1)
		s, b := evalAnd(tr, macros)
		if !b {
			return 0, false
		}
		sum = boolToInt(intToBool(sum) || intToBool(s))
	}
	return sum, true
}
// AndExpr = RelExpr *( '&&' RelExpr ) .
func evalAnd(tr *TokenReader, macros map[string]Macro) (int, bool) {
	sum, res := evalRel(tr, macros)
	if !res {
		return 0, false
	}
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	for tr.Get(0, ignore_flags).Kind==TKAndL {
		tr.Advance(1)
		s, b := evalRel(tr, macros)
		if !b {
			return 0, false
		}
		sum = boolToInt(intToBool(sum) && intToBool(s))
	}
	return sum, true
}
// RelExpr = AddExpr *( ( '==' | '!=' | '>=' | '<=' | '<' | '>' ) AddExpr ) .
func evalRel(tr *TokenReader, macros map[string]Macro) (int, bool) {
	sum, res := evalAdd(tr, macros)
	if !res {
		return 0, false
	}
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	for t := tr.Get(0, ignore_flags); t.Kind >= TKLess && t.Kind <= TKEq; t = tr.Get(0, ignore_flags) {
		k := t.Kind
		tr.Advance(1)
		s, b := evalAdd(tr, macros)
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
func evalAdd(tr *TokenReader, macros map[string]Macro) (int, bool) {
	sum, res := evalMul(tr, macros)
	if !res {
		return 0, false
	}
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	for t := tr.Get(0, ignore_flags); t.Kind==TKAdd || t.Kind==TKSub; t = tr.Get(0, ignore_flags) {
		k := t.Kind
		tr.Advance(1)
		s, b := evalMul(tr, macros)
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
func evalMul(tr *TokenReader, macros map[string]Macro) (int, bool) {
	sum, res := evalPrefix(tr, macros)
	if !res {
		return 0, false
	}
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	for t := tr.Get(0, ignore_flags); t.Kind==TKMul || t.Kind==TKDiv || t.Kind==TKMod; t = tr.Get(0, ignore_flags) {
		k := t.Kind
		tr.Advance(1)
		s, b := evalPrefix(tr, macros)
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
func evalPrefix(tr *TokenReader, macros map[string]Macro) (int, bool) {
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	switch t := tr.Get(0, ignore_flags); t.Kind {
	case TKNot:
		tr.Advance(1)
		val, res := evalPrefix(tr, macros)
		return boolToInt(!intToBool(val)), res
	case TKCompl:
		tr.Advance(1)
		val, res := evalPrefix(tr, macros)
		return ^val, res
	case TKSub:
		tr.Advance(1)
		val, res := evalPrefix(tr, macros)
		return -val, res
	default:
		return evalTerm(tr, macros)
	}
}
// Term = ident | 'defined' ident | integer | '(' Expr ')'.
func evalTerm(tr *TokenReader, macros map[string]Macro) (int, bool) {
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	switch t := tr.Get(0, ignore_flags); t.Kind {
	case TKDefined:
		///fmt.Printf("evalTerm :: got defined\n")
		tr.Advance(1)
		if tr.Get(0, ignore_flags).Kind != TKIdent {
			writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "'defined' expected identifier but got '%s'.", t.Lexeme)
			return 0, false
		}
		
		_, found := macros[tr.Get(0, ignore_flags).Lexeme]
		///fmt.Printf("evalTerm :: defined :: t: '%v' - found: %t\n", tr.Get(0, ignore_flags).ToString(), found)
		tr.Advance(1)
		return boolToInt(found), true
	case TKIdent:
		///time.Sleep(100 * time.Millisecond)
		///fmt.Printf("conditional preprocessing Macro: '%v'\n", t.ToString())
		
		m, found := macros[t.Lexeme]
		if !found {
			if t.Lexeme == "__LINE__" {
				return int(t.Line), true
			}
			writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "undefined symbol '%s'.", t.Lexeme)
			return 0, false
		}
		
		// if a Macro has no tokens at all, assign it a zero token.
		if len(m.Body) <= 0 {
			m.Body = []Token{PreprocZero}
		}
		
		expanded, good := m.Apply(tr)
		
		// skips past ( or ending ) if Macro function or skip name if Macro object.
		tr.Advance(1)
		
		if !good {
			///fmt.Printf("evalTerm :: ident not good\n")
			return 0, false
		} else {
			///fmt.Printf("evalTerm :: expanded: '%v' - current token: '%v'\n", expanded, tr.Get(0, ignore_flags))
			expanded_tr := MakeTokenReader(expanded)
			r, b := evalCond(&expanded_tr, macros)
			///fmt.Printf("evalTerm :: evaluated Macro result: %d\n", r)
			return r, b
		}
	case TKIntLit:
		///fmt.Printf("evalTerm :: got int lit '%s'\n", t.Lexeme)
		d, _ := strconv.ParseInt(t.Lexeme, 0, 64)
		tr.Advance(1)
		return int(d), true
	case TKLParen:
		///fmt.Printf("evalTerm :: got parentheses (expr)\n")
		tr.Advance(1)
		exp, success := evalCond(tr, macros)
		if tr.Get(0, ignore_flags).Kind != TKRParen {
			e := tr.Get(0, ignore_flags)
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "expected ')' got '%s' in conditional preprocessor t.", e.Lexeme)
			return 0, false
		} else if !success {
			e := tr.Get(0, ignore_flags)
			writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "parsing conditional preprocessor expression failed.")
			return 0, false
		} else {
			tr.Advance(1)
		}
		return exp, success
	default:
		e := tr.Get(0, ignore_flags)
		writeMsg(nil, os.Stdout, *e.Path, "syntax error", COLOR_RED, &e.Line, &e.Col, "conditional preprocessing requires a constant, integer expression, got '%s'.", e.Lexeme)
		return 0, false
	}
}


type (
	condInclCtx     int
	condInclStack []condInclCtx
)
const (
	IN_THEN = condInclCtx(iota)
	IN_ELIF
	IN_ELSE
)

func (stack *condInclStack) push(ctx condInclCtx) {
	*stack = append(*stack, ctx)
}

func (stack *condInclStack) peek() condInclCtx {
	if len(*stack) <= 0 {
		return condInclCtx(-1)
	}
	return (*stack)[len(*stack) - 1]
}

func (stack *condInclStack) pop() bool {
	if l := len(*stack); l <= 0 {
		return false
	} else {
		*stack = (*stack)[:l-1]
		return true
	}
}


func skipToNextLine(tr *TokenReader) bool {
	ignore_flags := TOKFLAG_IGNORE_COMMENT|TOKFLAG_IGNORE_SPACE|TOKFLAG_IGNORE_TAB
	for t := tr.Get(0, ignore_flags); tr.Idx < tr.Len() && t.Kind != TKEoF && t.Kind != TKNewline; t = tr.Get(0, ignore_flags) {
		///fmt.Printf("skipping to next line! '%v'\n", t.ToString())
		tr.Advance(1)
	}
	// skip past the newline as well.
	if t := tr.Get(0, ignore_flags); t.Kind==TKNewline {
		tr.Advance(1)
		///fmt.Printf("skipping past newline! '%v' | tr.Idx in bounds?: '%v'\n", tr.Get(0, ignore_flags).ToString(), tr.Idx < tr.Len())
		///fmt.Printf("tr.Tokens: '%p' | '%v'\n", &tr.Tokens, tr.Tokens)
		return true
	}
	return false
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
func skipToNextCondInclDirective(tr *TokenReader) {
	for t := tr.Get(0, TOKFLAG_IGNORE_ALL); tr.Idx < tr.Len() && t.Kind != TKEoF && !t.IsPreprocDirective(); t = tr.Get(0, TOKFLAG_IGNORE_ALL) {
		///fmt.Printf("skipping to next preproc directive '%v'\n", t.String())
		///time.Sleep(100 * time.Millisecond)
		if !skipToNextLine(tr) {
			break
		}
	}
}

func tokenizeBetweenCondInclDirective(tr *TokenReader) ([]Token, bool) {
	saved := tr.Idx
	///fmt.Printf("saved := tr.Idx: '%v' | current token: '%v'\n", saved, tr.Get(0, 0))
	goToNextCondIncl(tr)
	///fmt.Printf("tr.Idx: '%v'\n", tr.Idx)
	return tr.Tokens[saved : tr.Idx], true
}

// if the #if fails, we use this.
func goToNextCondIncl(tr *TokenReader) {
	nest, token_len := 0, tr.Len()
	for tr.Idx < token_len {
		///time.Sleep(100 * time.Millisecond)
		t := tr.Get(0, TOKFLAG_IGNORE_ALL)
		///fmt.Printf("goToNextCondIncl :: '%v'\n", t)
		if t.Kind >= TKPPIf && t.Kind <= TKPPEndIf {
			switch t.Kind {
			case TKPPIf:
				tr.Advance(1)
				///fmt.Printf("skipping over nested if\n")
				nest++
			case TKPPEndIf:
				if nest > 0 {
					tr.Advance(1)
					///fmt.Printf("skipping over nested endif\n")
					nest--
				} else if nest==0 {
					///fmt.Printf("went to next endif '%v'\n", t)
					return
				}
			case TKPPElse, TKPPElseIf:
				if nest==0 {
					///fmt.Printf("found '%v' - nest: '%v'\n", t, nest)
					return
				} else {
					tr.Advance(1)
				}
			}
		} else {
			tr.Advance(1)
			skipToNextCondInclDirective(tr)
		}
	}
}


func Preprocess(tokens []Token, flags int, macros map[string]Macro) ([]Token, bool) {
	if macros==nil {
		macros = make(map[string]Macro)
	}
	macros["__SPTOOLS__"] = Macro{Body: []Token{PreprocOne}, Params: nil, FuncLike: false}
	var ifStack condInclStack
	tr := MakeTokenReader(tokens)
	return preprocess(&tr, ifStack, macros, flags)
}


func preprocess(tr *TokenReader, ifStack condInclStack, macros map[string]Macro, flags int) ([]Token, bool) {
	var output []Token
	/*
	 * Design wise, we HAVE to loop through the tokens in a very controlled manner.
	 * The reason why is because we can't loop until EOF because preprocess is called
	 * recursively, especially on all tokens that macros make as macros can contain other macros.
	 */
	token_len := tr.Len()
	for tr.Idx < token_len {
		t := tr.Get(0, TOKFLAG_IGNORE_ALL)
		if t.Kind==TKEoF {
			output = append(output, t)
			break
		}
		///fmt.Printf("preprocessing t : '%v'\n", t.ToString())
		///time.Sleep(100 * time.Millisecond)
		if t.Kind==TKIdent {
			if m, found := macros[t.Lexeme]; found {
				if toks, res := m.Apply(tr); res {
					tr2 := MakeTokenReader(toks)
					toks, _ = preprocess(&tr2, ifStack, macros, flags)
					output = append(output, toks...)
				}
			} else {
				output = append(output, t)
				tr.Advance(1)
			}
		} else if t.IsPreprocDirective() {
			switch t.Kind {
			case TKPPErr:
				tr.Advance(1) // advance past the directive.
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				writeMsg(nil, os.Stdout, *t2.Path, "user error", COLOR_RED, &t2.Line, &t2.Col, "%s.", t2.Lexeme)
				return output, false
			case TKPPWarn:
				tr.Advance(1) // advance past the directive.
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				tr.Advance(1)
				writeMsg(nil, os.Stdout, *t2.Path, "user warning", COLOR_MAGENTA, &t2.Line, &t2.Col, "%s.", t2.Lexeme)
			case TKPPLine, TKPPPragma, TKPPFile:
				// skipping this for now.
				skipToNextLine(tr)
			case TKPPEndInput:
				///fmt.Printf("E N D I N G  I N P U T ----\n")
				goto preprocessing_done
			case TKPPDefine:
				tr.Advance(1) // advance past the directive.
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL) // get define name.
				tr.Advance(1)
				if t2.Kind != TKIdent {
					writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "token '%s' where expected identifier in #define.", t2.Lexeme)
					return output, false
				}
				if t3 := tr.Get(0, TOKFLAG_IGNORE_COMMENT); t3.Kind==TKLParen && t3.Col == ( t2.Col + uint32(len(t2.Lexeme)) ) {
					// function-like macro.
					tr.Advance(1)
					if m, good := MakeFuncMacro(tr); good {
						macros[t2.Lexeme] = m
					}
				} else {
					// object-like macro.
					m := MakeObjMacro(tr)
					macros[t2.Lexeme] = m
				}
			case TKPPUndef:
				tr.Advance(1) // advance past the directive.
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				if t2.Kind != TKIdent {
					writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "invalid '%s' name to undef.", t2.Lexeme)
					return output, false
				} else {
					delete(macros, t2.Lexeme)
					tr.Advance(1)
				}
			case TKPPInclude, TKPPTryInclude:
				tr.Advance(1) // advance past the directive.
				is_optional := t.Kind==TKPPTryInclude
				inc_file, filetext, read_err := "", "", ""
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				if t2.Kind==TKStrLit {
					tr.Advance(1)
					// local file
					if dir := filepath.Dir(*t2.Path); dir != "." {
						for dir != "." {
							inc_file = dir + string(filepath.Separator) + t2.Lexeme
							///fmt.Printf("inc_file loop: '%s'\n", inc_file)
							filetext, read_err = loadFile(inc_file)
							if filetext != "" {
								break
							} else {
								dir = filepath.Dir(dir)
							}
						}
					} else {
						inc_file = t2.Lexeme
						///fmt.Printf("inc_file: '%s'\n", inc_file)
						filetext, read_err = loadFile(inc_file)
						if filetext != "" {
							break
						}
					}
					
					/*
					 * Github Issue #5 - GammaCase
					 * when "" are used, it tries to lookup the file, relative to the .sp file first,
					 * and if it fails, it should fall back to include folders search as with <>,
					 * and that's where the difference with the original spcomp is,
					 * as currently it doesn't try to scan include folders as a fall back option.
					 */
					if filetext=="" {
						filedir := "include/" + t2.Lexeme
						filetext, read_err = loadFile(filedir)
					}
					if filetext=="" && !is_optional {
						writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "couldn't find file '%s'.", inc_file)
						return output, false
					}
				} else {
					// treat as include file.
					var str_path strings.Builder
					str_path.WriteString("include/")
					tr.Advance(1) // advance past the '<'.
					for tok_inc := tr.Get(0, TOKFLAG_IGNORE_ALL); tr.Idx < tr.Len() && tok_inc.Kind != TKGreater && tok_inc.Kind != TKEoF; tok_inc = tr.Get(0, TOKFLAG_IGNORE_ALL) {
						str_path.WriteString(tok_inc.Lexeme)
						tr.Advance(1)
					}
					tr.Advance(1)
					str_path.WriteString(".inc")
					inc_file = str_path.String()
					filetext, read_err = loadFile(inc_file)
					if filetext=="" && !is_optional {
						writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "couldn't find include file '%s'.", inc_file)
						return output, false
					}
				}
				
				inc_tokens := Tokenize(filetext, inc_file)
				include_tr := MakeTokenReader(inc_tokens)
				if preprocd, res := preprocess(&include_tr, ifStack, macros, flags); !res {
					writeMsg(nil, os.Stdout, *t2.Path, "inclusion error", COLOR_RED, &t2.Line, &t2.Col, "failed to preprocess '%s' -- read error?: '%s'.", inc_file, read_err)
					return output, false
				} else {
					preprocd = StripSpaceTokens(preprocd, flags & LEXFLAG_NEWLINES > 0)
					output = append(output, preprocd[:len(preprocd)-1]...)
				}
			case TKPPIf:
				tr.Advance(1) // advance past the directive.
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				ifStack.push(IN_THEN)
				if eval_res, success := evalCond(tr, macros); !success {
					writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "parsing conditional preprocessor failed.")
					return output, false
				} else {
					if eval_res != 0 {
						///fmt.Printf("#if eval_res from conditional t: '%d'\n", eval_res)
						///fmt.Printf("#if B4 line Skip -- Current Token:: '%v'\n", tr.Get(0, 0).ToString())
						skipToNextLine(tr)
						///fmt.Printf("#if After line Skip -- Current Token:: '%v'\n", tr.Get(0, 0).ToString())
						betweeners, tokenize_res := tokenizeBetweenCondInclDirective(tr)
						if !tokenize_res {
							writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "failed to tokenize between conditional preprocessing for #if.")
							return output, false
						}
						///fmt.Printf("#if betweeners:: '%v'\n", betweeners)
						between_tr := MakeTokenReader(betweeners)
						betweeners, _ = preprocess(&between_tr, ifStack, macros, flags)
						///fmt.Printf("#if betweeners preprocessed:: '%v'\n", betweeners)
						output = append(output, betweeners...)
						///fmt.Printf("#if after:: '%v'\n", tr.Get(0, TOKFLAG_IGNORE_COMMENT))
						
						///fmt.Printf("#if token i updated but before:: '%q'\n", tr.Get(0, TOKFLAG_IGNORE_COMMENT).ToString())
						for tr.Idx < tr.Len() && tr.Get(0, TOKFLAG_IGNORE_ALL).Kind != TKPPEndIf {
							///fmt.Printf("trying to get to #endif '%v'\n", tr.Get(0, TOKFLAG_IGNORE_ALL))
							goToNextCondIncl(tr)
							if tr.Get(0, TOKFLAG_IGNORE_ALL).Kind != TKPPEndIf {
								tr.Advance(1)
							}
						}
						///fmt.Printf("#if tr.Idx updated:: '%v' | '%v'\n", tr.Idx, tr.Get(0, TOKFLAG_IGNORE_COMMENT))
					} else {
						///fmt.Printf("#if failed, going to next conditional token, BEFORE current: '%v'\n", tr.Get(0, TOKFLAG_IGNORE_COMMENT))
						goToNextCondIncl(tr)
						///fmt.Printf("#if failed, going to next conditional token, AFTER current: '%v'\n", tr.Get(0, TOKFLAG_IGNORE_COMMENT))
					}
				}
			case TKPPEndIf:
				if res := ifStack.pop(); !res {
					writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "stray #endif.")
					return output, false
				}
				tr.Advance(1) // advance past the directive.
			case TKPPElse:
				///fmt.Printf("===============================#else directive\n")
				tr.Advance(1) // advance past the directive.
				if len(ifStack) <= 0 {
					writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "stray #else.", t.Lexeme)
					return output, false
				}
				ifStack.pop()
				ifStack.push(IN_ELSE)
				betweeners, tokenize_res := tokenizeBetweenCondInclDirective(tr)
				if !tokenize_res {
					writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "failed to tokenize between conditional preprocessing for #else.")
					return output, false
				}
				between_tr := MakeTokenReader(betweeners)
				///fmt.Printf("===============================#else betweeners '%v'\n", between_tr)
				betweeners, _ = preprocess(&between_tr, ifStack, macros, flags)
				output = append(output, betweeners...)
				///fmt.Printf("=============================== END #else directive\n")
			case TKPPElseIf:
				///fmt.Printf("===============================#elseif directive\n")
				tr.Advance(1) // advance past the directive.
				if len(ifStack) <= 0 {
					writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "stray #elseif.")
					return output, false
				} else if context := ifStack.peek(); context==IN_ELSE {
					writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "#elseif after #else.")
					return output, false
				}
				
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				ifStack.pop()
				ifStack.push(IN_ELIF)
				if eval_res, success := evalCond(tr, macros); !success {
					writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "parsing conditional preprocessor t failed.")
					return output, false
				} else {
					if eval_res != 0 {
						///fmt.Printf("#elseif eval_res from conditional t: '%d'\n", eval_res)
						skipToNextLine(tr)
						betweeners, tokenize_res := tokenizeBetweenCondInclDirective(tr)
						if !tokenize_res {
							writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "failed to tokenize between conditional preprocessing for #elseif.")
							return output, false
						}
						
						between_tr := MakeTokenReader(betweeners)
						///fmt.Printf("===============================#elseif betweeners '%v'\n", betweeners)
						betweeners, _ = preprocess(&between_tr, ifStack, macros, flags)
						///fmt.Printf("preprocessed betweeners '%v'\n", betweeners)
						output = append(output, betweeners...)
						
						for tr.Idx < tr.Len() && tr.Get(0, TOKFLAG_IGNORE_ALL).Kind != TKPPEndIf {
							///fmt.Printf("trying to get to #endif '%v'\n", tr.Get(0, TOKFLAG_IGNORE_ALL))
							goToNextCondIncl(tr)
							if tr.Get(0, TOKFLAG_IGNORE_ALL).Kind != TKPPEndIf {
								tr.Advance(1)
							}
						}
					} else {
						goToNextCondIncl(tr)
						///fmt.Printf("#elseif failed, going to next conditional t.\n")
						///fmt.Printf("#elseif fail post '%s'.\n", tr.Get(0, TOKFLAG_IGNORE_COMMENT).Lexeme)
					}
				}
				///fmt.Printf("===============================#elseif directive\n")
			case TKPPAssert:
				tr.Advance(1) // advance past the directive.
				t2 := tr.Get(0, TOKFLAG_IGNORE_ALL)
				if eval_res, success := evalCond(tr, macros); !success {
					writeMsg(nil, os.Stdout, *t2.Path, "syntax error", COLOR_RED, &t2.Line, &t2.Col, "parsing conditional preprocessor t failed.")
					return output, false
				} else if eval_res==0 {
					var preprocExpr strings.Builder
					for tr.Get(0, TOKFLAG_IGNORE_COMMENT).Kind != TKNewline {
						preprocExpr.WriteString(tr.Get(0, TOKFLAG_IGNORE_COMMENT).Lexeme)
						tr.Advance(1)
					}
					writeMsg(nil, os.Stdout, *t2.Path, "preprocess error", COLOR_RED, &t2.Line, &t2.Col, "assertion failed: %s", preprocExpr.String())
					return output, false
				}
			default:
				writeMsg(nil, os.Stdout, *t.Path, "syntax error", COLOR_RED, &t.Line, &t.Col, "unknown preprocessor token: t: '%s'.", t.Lexeme)
				return output, false
			}
		} else {
			output = append(output, t)
			tr.Advance(1)
		}
	}
preprocessing_done:
	return output, true
}
