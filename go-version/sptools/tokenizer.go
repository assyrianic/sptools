package SPTools

import (
	"strings"
	"unicode"
	//"fmt"
	"os"
)


const SPDigitSep = '_'
func isIden(c rune) bool {
	return unicode.IsLetter(c) || unicode.IsNumber(c) || c==SPDigitSep
}
func isAlphaNum(c rune) bool {
	return unicode.IsNumber(c) || unicode.IsLetter(c)
}
func isHex(c rune) bool {
	return unicode.IsNumber(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}
func isOctal(c rune) bool {
	return (c >= '0' && c <= '7')
}

type SPTokenKind int
const (
	SPTKEoF = SPTokenKind(iota)
	SPTKComment
	
	// used for preprocessor, removed afterwards.
	SPTKNewline
	
	// literal values
	SPTKIdent
	SPTKIntLit
	SPTKFloatLit
	SPTKStrLit
	SPTKCharLit
	
	// keywords
	// acquire, as, assert
	SPTKAcquire
	SPTKAs
	SPTKAssert
	
	// break, builtin
	SPTKBreak
	SPTKBuiltin
	
	// catch, case, cast_to, char, const, continue
	SPTKCatch
	SPTKCase
	SPTKCastTo
	SPTKChar
	SPTKConst
	SPTKContinue
	
	// decl, default, defined, delete, do, double
	SPTKDecl
	SPTKDefault
	SPTKDefined
	SPTKDelete
	SPTKDo
	SPTKDouble
	
	// else, enum, exit, explicit
	SPTKElse
	SPTKEnum
	SPTKExit
	SPTKExplicit
	
	// false, finally, for, foreach, forward, funcenum, functag, function
	SPTKFalse
	SPTKFinally
	SPTKFor
	SPTKForEach
	SPTKForward
	SPTKFuncEnum
	SPTKFuncTag
	SPTKFunction
	
	// goto
	SPTKGoto
	
	// if, implicit, import, in, int[8|16|32|64], interface, intn
	SPTKIf
	SPTKImplicit
	SPTKImport
	SPTKIn
	SPTKInt
	SPTKInt8
	SPTKInt16
	SPTKInt32
	SPTKInt64
	SPTKInterface
	SPTKIntN
	
	// let
	SPTKLet
	
	// methodmap
	SPTKMethodMap
	
	// namespace, native, new, null, __nullable__
	SPTKNameSpace
	SPTKNative
	SPTKNew
	SPTKNull
	SPTKNullable
	
	// object, operator
	SPTKObject
	SPTKOperator
	
	// package, private, protected, public
	SPTKPackage
	SPTKPrivate
	SPTKProtected
	SPTKPublic
	
	// readonly, return
	SPTKReadOnly
	SPTKReturn
	
	// sealed, sizeof, static, static_assert, stock, struct, switch
	SPTKSealed
	SPTKSizeof
	SPTKStatic
	SPTKStaticAssert
	SPTKStock
	SPTKStruct
	SPTKSwitch
	
	// this, throw, true, try, typedef, typeof, typeset
	SPTKThis
	SPTKThrow
	SPTKTrue
	SPTKTry
	SPTKTypedef
	SPTKTypeof
	SPTKTypeset
	
	// uint(8|16|32|64|n), union, using
	SPTKUInt8
	SPTKUInt16
	SPTKUInt32
	SPTKUInt64
	SPTKUnion
	SPTKUsing
	
	// var, variant, view_as, virtual, void, volatile
	SPTKVar
	SPTKVariant
	SPTKViewAs
	SPTKVirtual
	SPTKVoid
	SPTKVolatile
	
	// while, with
	SPTKWhile
	SPTKWith
	
	// preproc keywords
	// #assert #define #else #elseif #endif #endinput #endscript #error #warning #if #include #line #pragma #tryinclude #undef
	SPTKPPAssert
	SPTKPPDefine
	SPTKPPElse
	SPTKPPElseIf
	SPTKPPEndIf
	SPTKPPEndInput
	SPTKPPEndScript
	SPTKPPErr
	SPTKPPWarn
	SPTKPPIf
	SPTKPPInclude
	SPTKPPLine
	SPTKPPPragma
	SPTKPPTryInclude
	SPTKPPUndef
	
	// delimiters
	// ( ) [ ] { } , : ; #
	SPTKLParen
	SPTKRParen
	SPTKLBrack
	SPTKRBrack
	SPTKLCurl
	SPTKRCurl
	SPTKComma
	SPTKColon
	SPTKSemi
	SPTKHash
	
	// operators
	// + - * / % ! . .. ...
	SPTKAdd
	SPTKSub
	SPTKMul
	SPTKDiv
	SPTKMod
	SPTKNot
	SPTKDot
	SPTK2Dots
	SPTKEllipses
	
	// & | ^ ~ << >> >>>
	SPTKAnd
	SPTKOr
	SPTKXor
	SPTKCompl
	SPTKShAL
	SPTKShAR
	SPTKShLR
	
	// < > >= <= != == && ||
	SPTKLess
	SPTKGreater
	SPTKGreaterE
	SPTKLessE
	SPTKNotEq
	SPTKEq
	SPTKAndL
	SPTKOrL
	
	// = += -= *= /= %= &= |= ^= <<= >>= >>>=
	SPTKAssign
	SPTKAddA
	SPTKSubA
	SPTKMulA
	SPTKDivA
	SPTKModA
	SPTKAndA
	SPTKOrA
	SPTKXorA
	SPTKShALA
	SPTKShARA
	SPTKShLRA
	
	// ++ -- :: ?
	SPTKIncr
	SPTKDecr
	SPTK2Colons
	SPTKQMark
	
	SPTKMaxTokens
)


var (
	Keywords = map[string]SPTokenKind {
		"acquire": SPTKAcquire,
		"as": SPTKAs,
		"assert": SPTKAssert,
		"break": SPTKBreak,
		"builtin": SPTKBuiltin,
		"catch": SPTKCatch,
		"case": SPTKCase,
		"cast_to": SPTKCastTo,
		"char": SPTKChar,
		"const": SPTKConst,
		"continue": SPTKContinue,
		"decl": SPTKDecl,
		"default": SPTKDefault,
		"defined": SPTKDefined,
		"delete": SPTKDelete,
		"do": SPTKDo,
		"double": SPTKDouble,
		"else": SPTKElse,
		"enum": SPTKEnum,
		"exit": SPTKExit,
		"explicit": SPTKExplicit,
		"false": SPTKFalse,
		"finally": SPTKFinally,
		"for": SPTKFor,
		"foreach": SPTKForEach,
		"forward": SPTKForward,
		"funcenum": SPTKFuncEnum,
		"functag": SPTKFuncTag,
		"function": SPTKFunction,
		"goto": SPTKGoto,
		"if": SPTKIf,
		"implicit": SPTKImplicit,
		"import": SPTKImport,
		"in": SPTKIn,
		"int": SPTKInt,
		"int8": SPTKInt8,
		"int16": SPTKInt16,
		"int32": SPTKInt32,
		"int64": SPTKInt64,
		"interface": SPTKInterface,
		"intn": SPTKIntN,
		"let": SPTKLet,
		"methodmap": SPTKMethodMap,
		"namespace": SPTKNameSpace,
		"native": SPTKNative,
		"new": SPTKNew,
		"null": SPTKNull,
		"__nullable__": SPTKNullable,
		"object": SPTKObject,
		"operator": SPTKOperator,
		"package": SPTKPackage,
		"private": SPTKPrivate,
		"protected": SPTKProtected,
		"public": SPTKPublic,
		"readonly": SPTKReadOnly,
		"return": SPTKReturn,
		"sealed": SPTKSealed,
		"sizeof": SPTKSizeof,
		"static": SPTKStatic,
		"static_assert": SPTKStaticAssert,
		"stock": SPTKStock,
		"struct": SPTKStruct,
		"switch": SPTKSwitch,
		"this": SPTKThis,
		"throw": SPTKThrow,
		"true": SPTKTrue,
		"try": SPTKTry,
		"typedef": SPTKTypedef,
		"typeof": SPTKTypeof,
		"typesef": SPTKTypeset,
		"uint8": SPTKUInt8,
		"uint16": SPTKUInt16,
		"uint32": SPTKUInt32,
		"uint64": SPTKUInt64,
		"union": SPTKUnion,
		"using": SPTKUsing,
		"var": SPTKVar,
		"variant": SPTKVariant,
		"view_as": SPTKViewAs,
		"virtual": SPTKVirtual,
		"void": SPTKVoid,
		"volatile": SPTKVolatile,
		"while": SPTKWhile,
		"with": SPTKWith,
	}
	Opers = map[string]SPTokenKind {
		"(": SPTKLParen,
		")": SPTKRParen,
		"[": SPTKLBrack,
		"]": SPTKRBrack,
		"{": SPTKLCurl,
		"}": SPTKRCurl,
		",": SPTKComma,
		":": SPTKColon,
		";": SPTKSemi,
		"#": SPTKHash,
		"+": SPTKAdd,
		"-": SPTKSub,
		"*": SPTKMul,
		"/": SPTKDiv,
		"%": SPTKMod,
		"!": SPTKNot,
		".": SPTKDot,
		"..": SPTK2Dots,
		"...": SPTKEllipses,
		"&": SPTKAnd,
		"|": SPTKOr,
		"^": SPTKXor,
		"~": SPTKCompl,
		"<<": SPTKShAL,
		">>": SPTKShAR,
		">>>": SPTKShLR,
		"<": SPTKLess,
		">": SPTKGreater,
		">=": SPTKGreaterE,
		"<=": SPTKLessE,
		"!=": SPTKNotEq,
		"==": SPTKEq,
		"&&": SPTKAndL,
		"||": SPTKOrL,
		"=": SPTKAssign,
		"+=": SPTKAddA,
		"-=": SPTKSubA,
		"*=": SPTKMulA,
		"/=": SPTKDivA,
		"%=": SPTKModA,
		"&=": SPTKAndA,
		"|=": SPTKOrA,
		"^=": SPTKXorA,
		"<<=": SPTKShALA,
		">>=": SPTKShARA,
		">>>=": SPTKShLRA,
		"++": SPTKIncr,
		"--": SPTKDecr,
		"::": SPTK2Colons,
		"?": SPTKQMark,
	}
)


type SPToken struct {
	Lexeme    string
	Path     *string
	Line, Col int
	Kind      SPTokenKind
}

func (tok *SPToken) IsKeyword() bool {
	return tok.Kind >= SPTKAcquire && tok.Kind <= SPTKWith
}

func (tok *SPToken) IsLiteral() bool {
	return tok.Kind >= SPTKIdent && tok.Kind <= SPTKCharLit
}

func (tok *SPToken) IsDelimiter() bool {
	return tok.Kind >= SPTKLParen && tok.Kind <= SPTKRCurl
}

func (tok *SPToken) IsOperator() bool {
	return tok.Kind >= SPTKAdd && tok.Kind <= SPTK2Colons
}

/*
type spScanner struct {
	runes                []rune
	filename             string
	idx, line, lineStart int
	max, num_msgs        int
}
*/

func lexBinary(runes []rune, idx, max int, filename string) (int, bool) {
	if idx >= max || runes[idx] != '0' {
		return idx, false
	} else if idx + 1 < max && (runes[idx+1] != 'b' && runes[idx+1] != 'B') {
		return idx, false
	}
	
	idx += 2
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==SPDigitSep) {
		switch runes[idx] {
			case '0', '1', SPDigitSep:
				idx++
			default:
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "bad digit %c in binary literal", runes[idx])
				return idx, false
		}
	}
	return idx, true
}

func lexHex(runes []rune, idx, max int, filename string) (int, bool) {
	if max <= idx || runes[idx] != '0' {
		return idx, false
	} else if idx + 1 < max && (runes[idx + 1] != 'x' && runes[idx + 1] != 'X') {
		return idx, false
	}
	idx += 2
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==SPDigitSep) {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', SPDigitSep:
				fallthrough
			case 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F':
				idx++
			default:
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "bad digit %c in hex literal", runes[idx])
				return idx, false
		}
	}
	return idx, true
}

func lexOctal(runes []rune, idx, max int, filename string) (int, bool) {
	if max <= idx || runes[idx] != '0' {
		return idx, false
	} else if idx + 1 < max && (runes[idx + 1] != 'o' && runes[idx + 1] != 'O') {
		return idx, false
	}
	idx += 2
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==SPDigitSep) {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', SPDigitSep:
				idx++
			default:
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "bad digit %c in octal literal", runes[idx])
				return idx, false
		}
	}
	return idx, true
}

func lexDecimal(runes []rune, idx, max int, filename string) (int, bool, bool) {
	start := idx
	if max <= idx {
		return idx, false, false
	}
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==SPDigitSep || runes[idx]=='.') {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', SPDigitSep:
				idx++
			case '.':
				return lexFloat(runes, start, max, filename)
			default:
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "bad digit %c in decimal literal", runes[idx])
				return idx, false, false
		}
	}
	return idx, true, false
}

func lexFloat(runes []rune, idx, max int, filename string) (int, bool, bool) {
	if max <= idx {
		return idx, false, true
	}
	var got_num, got_E, num_after_E, got_math bool
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==SPDigitSep || runes[idx]=='.' || runes[idx]=='+' || runes[idx]=='-') {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				if !got_num {
					got_num = true
				}
				if got_E && !num_after_E {
					num_after_E = true
				}
				idx++
			case SPDigitSep:
				idx++
			case '.':
				if !got_num {
					writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "'.' in float literal before numbers.")
					return idx, false, true
				}
				idx++
			case 'e':
				if got_E {
					writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "too many Es in float.")
					return idx, false, true
				}
				got_E = true
				idx++
			case '+', '-':
				if num_after_E || got_math {
					return idx, true, true
				} else if got_E && idx+1 < max && !unicode.IsDigit(runes[idx+1]) {
					writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "missing numbers after +/- in E exponent.")
					return idx, false, true
				}
				got_math = true
				idx++
			default:
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "bad digit %c in float literal.", runes[idx])
				return idx, false, true
		}
	}
	if got_E && !num_after_E {
		writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, nil, nil, "exponent E is missing numbers in float literal.")
		return idx, false, true
	}
	return idx, true, true
}


func Tokenize(src, filename string) []SPToken {
	var (
		tokens []SPToken
		idx, start int
	)
	line, max, runes := 1, len(src), ([]rune)(src)
	for idx < max {
		if c := runes[idx]; unicode.IsSpace(c) {
			idx++
			if c=='\n' {
				tokens = append(tokens, SPToken{Lexeme: "\n", Path: &filename, Line: line, Col: idx - start, Kind: SPTKNewline})
				line++
				start = idx
			}
		} else if unicode.IsLetter(c) || c=='_' {
			// handle identifiers & keywords.
			col, starting := idx - start, idx
			for idx < max && isIden(runes[idx]) {
				idx++
			}
			lexeme := string(runes[starting : idx])
			if tkind, found := Keywords[lexeme]; found {
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: line, Col: col, Kind: tkind})
			} else {
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: line, Col: col, Kind: SPTKIdent})
			}
		} else if c=='/' && idx + 1 < max && runes[idx + 1]=='/' {
			// single line comment.
			col, starting, starting_line := idx - start, idx, line
			idx += 2
			for idx < max && runes[idx] != '\n' {
				if runes[idx]=='\\' {
					line++
					idx++
					start = idx
				}
				idx++
			}
			lexeme := string(runes[starting : idx])
			tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: SPTKComment})
		} else if c=='/' && idx + 1 < max && runes[idx + 1]=='*' {
			// multi-line comment.
			col, starting, starting_line := idx - start, idx, line
			idx += 2
			stop := false
			for idx < max && !stop {
				if runes[idx]=='\n' {
					line++
					start = idx
				} else if runes[idx]=='*' && idx + 1 < max && runes[idx + 1]=='/' {
					idx++
					stop = true
				}
				idx++
			}
			lexeme := string(runes[starting : idx])
			tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: SPTKComment})
		} else if unicode.IsNumber(c) {
			// handle numbers.
			col, starting, starting_line := idx - start, idx, line
			if new_idx, result := lexBinary(runes, idx, max, filename); result {
				idx = new_idx
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: SPTKIntLit})
			} else if new_idx, result = lexHex(runes, idx, max, filename); result {
				idx = new_idx
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: SPTKIntLit})
			} else if new_idx, result = lexOctal(runes, idx, max, filename); result {
				idx = new_idx
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: SPTKIntLit})
			} else if nidx, res, is_float := lexDecimal(runes, idx, max, filename); res {
				idx = nidx
				var kind SPTokenKind
				if is_float {
					kind = SPTKFloatLit
				} else {
					kind = SPTKIntLit
				}
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: kind})
			} else {
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, &line, &col, "failed to tokenize number.")
				goto errored_return
			}
		} else if c=='"' || c=='\'' {
			col, starting_line, q := idx - start, line, c
			idx++
			var b strings.Builder
			for idx < max && runes[idx] != q {
				if runes[idx]=='\\' {
					idx++
					switch esc := runes[idx]; esc {
						case '\'', '"':
							b.WriteRune(esc)
							idx++
						case '\\':
							b.WriteRune('\\')
							idx++
						case 'a':
							b.WriteRune('\a')
							idx++
						case 'r':
							b.WriteRune('\r')
							idx++
						case 'b':
							b.WriteRune('\b')
							idx++
						case 't':
							b.WriteRune('\t')
							idx++
						case 'v':
							b.WriteRune('\v')
							idx++
						case 'n':
							b.WriteRune('\n')
							idx++
						case 'f':
							b.WriteRune('\f')
							idx++
						case 'X', 'x':
							idx++
							value := func() rune {
								if idx < max && !isHex(runes[idx]) {
									return -1
								}
								var r rune
								for idx < max {
									switch runes[idx] {
										case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
											r = (r << 4) | (c - '0')
										case 'A', 'B', 'C', 'D', 'E', 'F':
											r = (r << 4) | (c - 'a' + 10)
										case 'a', 'b', 'c', 'd', 'e', 'f':
											r = (r << 4) | (c - 'A' + 10)
										default:
											return r
									}
									idx++
								}
								return r
							}()
							b.WriteRune(value)
						case 'u', 'U':
							idx++
							value := func(is_u32 bool) rune {
								var r rune
								var encoding_size int
								if is_u32 {
									encoding_size = 8
								} else {
									encoding_size = 4
								}
								for n:=0; idx < max && n < encoding_size; n++ {
									switch runes[idx] {
										case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
											r = (r << 4) | (c - '0')
										case 'A', 'B', 'C', 'D', 'E', 'F':
											r = (r << 4) | (c - 'a' + 10)
										case 'a', 'b', 'c', 'd', 'e', 'f':
											r = (r << 4) | (c - 'A' + 10)
										default:
											return r
									}
									idx++
								}
								if !unicode.IsPrint(r) {
									return -1
								}
								return r
							}(esc=='U')
							b.WriteRune(value)
						case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
							value := func() rune {
								if idx < max && !isOctal(runes[idx]) {
									return -1
								}
								var r rune
								for idx < max {
									switch runes[idx] {
										case '0', '1', '2', '3', '4', '5', '6', '7':
											r = (r << 3) | (c - '0')
										default:
											return r
									}
									idx++
								}
								return r
							}()
							b.WriteRune(value)
					}
				} else {
					b.WriteRune(runes[idx])
					idx++
				}
			}
			idx++
			var kind SPTokenKind
			if q=='"' {
				kind = SPTKStrLit
			} else {
				kind = SPTKCharLit
			}
			tokens = append(tokens, SPToken{Lexeme: b.String(), Path: &filename, Line: starting_line, Col: col, Kind: kind})
		} else {
			col, starting, starting_line := idx - start, idx, line
			oper_size, oper_key, got_match := 0, "", false
			for key := range Opers {
				// Match largest operator first.
				keylen := len(key)
				if idx + keylen > max {
					continue
				}
				
				if string(runes[starting : idx+keylen])==key && oper_size < keylen {
					oper_size, oper_key, got_match = keylen, key, true
				}
			}
			
			if got_match {
				idx += len(oper_key)
				lexeme, kind := string(runes[starting : idx]), Opers[oper_key]
				tokens = append(tokens, SPToken{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: kind})
				continue
			} else {
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, &line, &col, "unknown operator: '%c'.", runes[starting])
				goto errored_return
			}
		}
	}
errored_return:
	tokens = append(tokens, SPToken{Lexeme: "<eof>", Path: &filename, Line: line, Col: idx - start, Kind: SPTKEoF})
	return tokens
}

func ConcatStringLiterals(tokens []SPToken) []SPToken {
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		if tokens[i].Kind==SPTKStrLit && i + 1 < num_tokens && tokens[i+1].Kind==SPTKEllipses && i + 2 < num_tokens && tokens[i+2].Kind==SPTKStrLit {
			// merge the two strings together, then remove the ... and 2nd string from the token list.
			saved := i - 1
			tokens[i].Lexeme += tokens[i+2].Lexeme
			tokens = append(tokens[:i+1], tokens[i+3:]...)
			num_tokens = len(tokens)
			i = saved
		}
	}
	return tokens
}

func StripNewlineTokens(tokens []SPToken) []SPToken {
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		if tokens[i].Kind==SPTKNewline {
			tokens = append(tokens[:i], tokens[i+1:]...)
			num_tokens = len(tokens)
			i = 0
		}
	}
	return tokens
}

func RemoveComments(tokens []SPToken) []SPToken {
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		if tokens[i].Kind==SPTKComment {
			tokens = append(tokens[:i], tokens[i+1:]...)
			num_tokens = len(tokens)
			i = 0
		}
	}
	return tokens
}