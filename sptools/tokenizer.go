package SPTools

import (
	"strings"
	"unicode"
	//"fmt"
	"os"
	"unicode/utf8"
)


const DigitSep = '_'
func isIden(c rune) bool {
	return unicode.IsLetter(c) || unicode.IsNumber(c) || c==DigitSep
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

type TokenKind uint8
const (
	TKEoF = TokenKind(iota)
	TKComment
	
	// used for preprocessor, removed afterwards.
	TKNewline
	
	// literal values
	TKIdent
	TKIntLit
	TKFloatLit
	TKStrLit
	TKCharLit
	
	// keywords
	// acquire, any, as, assert
	TKAcquire
	TKAs
	TKAssert
	
	// break, builtin
	TKBreak
	TKBuiltin
	
	// catch, case, cast_to, char, const, continue
	TKCatch
	TKCase
	TKCastTo
	TKChar
	TKConst
	TKContinue
	
	// decl, default, defined, delete, do, double
	TKDecl
	TKDefault
	TKDefined
	TKDelete
	TKDo
	TKDouble
	
	// else, enum, exit, explicit
	TKElse
	TKEnum
	TKExit
	TKExplicit
	
	// false, finally, for, foreach, forward, funcenum, functag, function
	TKFalse
	TKFinally
	TKFor
	TKForEach
	TKForward
	TKFuncEnum
	TKFuncTag
	TKFunction
	
	// goto
	TKGoto
	
	// if, implicit, import, in, int[8|16|32|64], intn, interface
	TKIf
	TKImplicit
	TKImport
	TKIn
	TKInt
	TKInt8
	TKInt16
	TKInt32
	TKInt64
	TKIntN
	TKInterface
	
	// let
	TKLet
	
	// methodmap
	TKMethodMap
	
	// namespace, native, new, null, __nullable__
	TKNameSpace
	TKNative
	TKNew
	TKNull
	TKNullable
	
	// object, operator
	TKObject
	TKOperator
	
	// package, private, protected, public
	TKPackage
	TKPrivate
	TKProtected
	TKPublic
	
	// readonly, return
	TKReadOnly
	TKReturn
	
	// sealed, sizeof, static, static_assert, stock, struct, switch
	TKSealed
	TKSizeof
	TKStatic
	TKStaticAssert
	TKStock
	TKStruct
	TKSwitch
	
	// this, throw, true, try, typedef, typeof, typeset
	TKThis
	TKThrow
	TKTrue
	TKTry
	TKTypedef
	TKTypeof
	TKTypeset
	
	// uint(8|16|32|64|n), union, using
	TKUInt8
	TKUInt16
	TKUInt32
	TKUInt64
	TKUnion
	TKUsing
	
	// var, variant, view_as, virtual, void, volatile
	TKVar
	TKVariant
	TKViewAs
	TKVirtual
	TKVoid
	TKVolatile
	
	// while, with
	TKWhile
	TKWith
	
	// preproc keywords
	// #assert #define #else #elseif #endif #endinput #endscript #error #warning #if #include #line #pragma #tryinclude #undef
	/*
	TKPPAssert
	TKPPDefine
	TKPPElse
	TKPPElseIf
	TKPPEndIf
	TKPPEndInput
	TKPPEndScript
	TKPPErr
	TKPPWarn
	TKPPIf
	TKPPInclude
	TKPPLine
	TKPPPragma
	TKPPTryInclude
	TKPPUndef
	*/
	
	// delimiters
	// ( ) [ ] { } , : ; #
	TKLParen
	TKRParen
	TKLBrack
	TKRBrack
	TKLCurl
	TKRCurl
	TKComma
	TKColon
	TKSemi
	TKHash
	
	// operators
	// + - * / % ! . .. ...
	TKAdd
	TKSub
	TKMul
	TKDiv
	TKMod
	TKNot
	TKDot
	TK2Dots
	TKEllipses
	
	// & | ^ ~ << >> >>>
	TKAnd
	TKOr
	TKXor
	TKCompl
	TKShAL
	TKShAR
	TKShLR
	
	// < > >= <= != == && ||
	TKLess
	TKGreater
	TKGreaterE
	TKLessE
	TKNotEq
	TKEq
	TKAndL
	TKOrL
	
	// = += -= *= /= %= &= |= ^= <<= >>= >>>=
	TKAssign
	TKAddA
	TKSubA
	TKMulA
	TKDivA
	TKModA
	TKAndA
	TKOrA
	TKXorA
	TKShALA
	TKShARA
	TKShLRA
	
	// ++ -- :: ?
	TKIncr
	TKDecr
	TK2Colons
	TKQMark
	
	TKMaxTokens
)


var (
	Keywords = map[string]TokenKind {
		"acquire": TKAcquire,
		"as": TKAs,
		"assert": TKAssert,
		"break": TKBreak,
		"builtin": TKBuiltin,
		"catch": TKCatch,
		"case": TKCase,
		"cast_to": TKCastTo,
		"char": TKChar,
		"const": TKConst,
		"continue": TKContinue,
		"decl": TKDecl,
		"default": TKDefault,
		"defined": TKDefined,
		"delete": TKDelete,
		"do": TKDo,
		"double": TKDouble,
		"else": TKElse,
		"enum": TKEnum,
		"exit": TKExit,
		"explicit": TKExplicit,
		"false": TKFalse,
		"finally": TKFinally,
		"for": TKFor,
		"foreach": TKForEach,
		"forward": TKForward,
		"funcenum": TKFuncEnum,
		"functag": TKFuncTag,
		"function": TKFunction,
		"goto": TKGoto,
		"if": TKIf,
		"implicit": TKImplicit,
		"import": TKImport,
		"in": TKIn,
		"int": TKInt,
		"int8": TKInt8,
		"int16": TKInt16,
		"int32": TKInt32,
		"int64": TKInt64,
		"interface": TKInterface,
		"intn": TKIntN,
		"let": TKLet,
		"methodmap": TKMethodMap,
		"namespace": TKNameSpace,
		"native": TKNative,
		"new": TKNew,
		"null": TKNull,
		"__nullable__": TKNullable,
		"object": TKObject,
		"operator": TKOperator,
		"package": TKPackage,
		"private": TKPrivate,
		"protected": TKProtected,
		"public": TKPublic,
		"readonly": TKReadOnly,
		"return": TKReturn,
		"sealed": TKSealed,
		"sizeof": TKSizeof,
		"static": TKStatic,
		"static_assert": TKStaticAssert,
		"stock": TKStock,
		"struct": TKStruct,
		"switch": TKSwitch,
		"this": TKThis,
		"throw": TKThrow,
		"true": TKTrue,
		"try": TKTry,
		"typedef": TKTypedef,
		"typeof": TKTypeof,
		"typesef": TKTypeset,
		"uint8": TKUInt8,
		"uint16": TKUInt16,
		"uint32": TKUInt32,
		"uint64": TKUInt64,
		"union": TKUnion,
		"using": TKUsing,
		"var": TKVar,
		"variant": TKVariant,
		"view_as": TKViewAs,
		"virtual": TKVirtual,
		"void": TKVoid,
		"volatile": TKVolatile,
		"while": TKWhile,
		"with": TKWith,
	}
	Opers = map[string]TokenKind {
		"(": TKLParen,
		")": TKRParen,
		"[": TKLBrack,
		"]": TKRBrack,
		"{": TKLCurl,
		"}": TKRCurl,
		",": TKComma,
		":": TKColon,
		";": TKSemi,
		"#": TKHash,
		"+": TKAdd,
		"-": TKSub,
		"*": TKMul,
		"/": TKDiv,
		"%": TKMod,
		"!": TKNot,
		".": TKDot,
		"..": TK2Dots,
		"...": TKEllipses,
		"&": TKAnd,
		"|": TKOr,
		"^": TKXor,
		"~": TKCompl,
		"<<": TKShAL,
		">>": TKShAR,
		">>>": TKShLR,
		"<": TKLess,
		">": TKGreater,
		">=": TKGreaterE,
		"<=": TKLessE,
		"!=": TKNotEq,
		"==": TKEq,
		"&&": TKAndL,
		"||": TKOrL,
		"=": TKAssign,
		"+=": TKAddA,
		"-=": TKSubA,
		"*=": TKMulA,
		"/=": TKDivA,
		"%=": TKModA,
		"&=": TKAndA,
		"|=": TKOrA,
		"^=": TKXorA,
		"<<=": TKShALA,
		">>=": TKShARA,
		">>>=": TKShLRA,
		"++": TKIncr,
		"--": TKDecr,
		"::": TK2Colons,
		"?": TKQMark,
	}
	TokenToStr = [...]string {
		TKEoF: "<end-of-file>",
		TKComment: "<comment>",
		TKNewline: "<newline>",
		TKIdent: "<identifier>",
		TKIntLit: "<integer>",
		TKFloatLit: "<float>",
		TKStrLit: "<string>",
		TKCharLit: "<char>",
		TKAcquire: "acquire",
		TKAs: "as",
		TKAssert: "assert",
		TKBreak: "break",
		TKBuiltin: "builtin",
		TKCatch: "catch",
		TKCase: "case",
		TKCastTo: "cast_to",
		TKChar: "char",
		TKConst: "const",
		TKContinue: "continue",
		TKDecl: "decl",
		TKDefault: "default",
		TKDefined: "defined",
		TKDelete: "delete",
		TKDo: "do",
		TKDouble: "double",
		TKElse: "else",
		TKEnum: "enum",
		TKExit: "exit",
		TKExplicit: "explicit",
		TKFalse: "false",
		TKFinally: "finally",
		TKFor: "for",
		TKForEach: "foreach",
		TKForward: "forward",
		TKFuncEnum: "funcenum",
		TKFuncTag: "functag",
		TKFunction: "function",
		TKGoto: "goto",
		TKIf: "if",
		TKImplicit: "implicit",
		TKImport: "import",
		TKIn: "in",
		TKInt: "int",
		TKInt8: "int8",
		TKInt16: "int16",
		TKInt32: "int32",
		TKInt64: "int64",
		TKInterface: "interface",
		TKIntN: "intn",
		TKLet: "let",
		TKMethodMap: "methodmap",
		TKNameSpace: "namespace",
		TKNative: "native",
		TKNew: "new",
		TKNull: "null",
		TKNullable: "__nullable__",
		TKObject: "object",
		TKOperator: "operator",
		TKPackage: "package",
		TKPrivate: "private",
		TKProtected: "protected",
		TKPublic: "public",
		TKReadOnly: "readonly",
		TKReturn: "return",
		TKSealed: "sealed",
		TKSizeof: "sizeof",
		TKStatic: "static",
		TKStaticAssert: "static_assert",
		TKStock: "stock",
		TKStruct: "struct",
		TKSwitch: "switch",
		TKThis: "this",
		TKThrow: "throw",
		TKTrue: "true",
		TKTry: "try",
		TKTypedef: "typedef",
		TKTypeof: "typeof",
		TKTypeset: "typesef",
		TKUInt8: "uint8",
		TKUInt16: "uint16",
		TKUInt32: "uint32",
		TKUInt64: "uint64",
		TKUnion: "union",
		TKUsing: "using",
		TKVar: "var",
		TKVariant: "variant",
		TKViewAs: "view_as",
		TKVirtual: "virtual",
		TKVoid: "void",
		TKVolatile: "volatile",
		TKWhile: "while",
		TKWith: "with",
		// operators time.
		TKLParen: "(",
		TKRParen: ")",
		TKLBrack: "[",
		TKRBrack: "]",
		TKLCurl: "{",
		TKRCurl: "}",
		TKComma: ",",
		TKColon: ":",
		TKSemi: ";",
		TKHash: "#",
		TKAdd: "+",
		TKSub: "-",
		TKMul: "*",
		TKDiv: "/",
		TKMod: "%",
		TKNot: "!",
		TKDot: ".",
		TK2Dots: "..",
		TKEllipses: "...",
		TKAnd: "&",
		TKOr: "|",
		TKXor: "^",
		TKCompl: "~",
		TKShAL: "<<",
		TKShAR: ">>",
		TKShLR: ">>>",
		TKLess: "<",
		TKGreater: ">",
		TKGreaterE: ">=",
		TKLessE: "<=",
		TKNotEq: "!=",
		TKEq: "==",
		TKAndL: "&&",
		TKOrL: "||",
		TKAssign: "=",
		TKAddA: "+=",
		TKSubA: "-=",
		TKMulA: "*=",
		TKDivA: "/=",
		TKModA: "%=",
		TKAndA: "&=",
		TKOrA: "|=",
		TKXorA: "^=",
		TKShALA: "<<=",
		TKShARA: ">>=",
		TKShLRA: ">>>=",
		TKIncr: "++",
		TKDecr: "--",
		TK2Colons: "::",
		TKQMark: "?",
	}
)


type Token struct {
	Lexeme    string
	Path     *string
	Line, Col int
	Kind      TokenKind
}

func (tok Token) IsKeyword() bool {
	return tok.Kind >= TKAcquire && tok.Kind <= TKWith
}

func (tok Token) IsLiteral() bool {
	return tok.Kind >= TKIdent && tok.Kind <= TKCharLit
}

func (tok Token) IsDelimiter() bool {
	return tok.Kind >= TKLParen && tok.Kind <= TKRCurl
}

func (tok Token) IsOperator() bool {
	return tok.Kind >= TKAdd && tok.Kind <= TK2Colons
}

func (tok Token) IsType() bool {
	switch tok.Kind {
		case TKInt, TKInt8, TKInt16, TKInt32, TKInt64, TKIntN:
			fallthrough
		case TKUInt8, TKUInt16, TKUInt32, TKUInt64, TKChar, TKDouble, TKVoid:
			return true
		default:
			return false
	}
}

func (tok Token) IsStorageClass() bool {
	switch tok.Kind {
		case TKConst, TKStock, TKPublic, TKPrivate, TKProtected, TKStatic:
			fallthrough
		case TKForward, TKNative, TKReadOnly, TKSealed, TKVirtual:
			return true
		default:
			return false
	}
}


/*
type scanner struct {
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
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==DigitSep) {
		switch runes[idx] {
			case '0', '1', DigitSep:
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
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==DigitSep) {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', DigitSep:
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
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==DigitSep) {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', DigitSep:
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
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==DigitSep || runes[idx]=='.') {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', DigitSep:
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
	for idx < max && (isAlphaNum(runes[idx]) || runes[idx]==DigitSep || runes[idx]=='.' || runes[idx]=='+' || runes[idx]=='-') {
		switch runes[idx] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				if !got_num {
					got_num = true
				}
				if got_E && !num_after_E {
					num_after_E = true
				}
				idx++
			case DigitSep:
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


func Tokenize(src, filename string) []Token {
	var (
		tokens []Token
		idx, start int
	)
	line, max, runes := 1, len(src), ([]rune)(src)
	for idx < max {
		if c := runes[idx]; unicode.IsSpace(c) {
			idx++
			if c=='\n' {
				tokens = append(tokens, Token{Lexeme: "\n", Path: &filename, Line: line, Col: idx - start, Kind: TKNewline})
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
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: line, Col: col, Kind: tkind})
			} else {
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: line, Col: col, Kind: TKIdent})
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
			tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKComment})
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
			tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKComment})
		} else if unicode.IsNumber(c) {
			// handle numbers.
			col, starting, starting_line := idx - start, idx, line
			if new_idx, result := lexBinary(runes, idx, max, filename); result {
				idx = new_idx
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKIntLit})
			} else if new_idx, result = lexHex(runes, idx, max, filename); result {
				idx = new_idx
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKIntLit})
			} else if new_idx, result = lexOctal(runes, idx, max, filename); result {
				idx = new_idx
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKIntLit})
			} else if nidx, res, is_float := lexDecimal(runes, idx, max, filename); res {
				idx = nidx
				var kind TokenKind
				if is_float {
					kind = TKFloatLit
				} else {
					kind = TKIntLit
				}
				lexeme := string(runes[starting : idx])
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: kind})
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
						case '\n':
							idx++
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
									switch chr := runes[idx]; chr {
										case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
											r = (r << 4) | (chr - '0')
										case 'A', 'B', 'C', 'D', 'E', 'F':
											r = (r << 4) | (chr - 'a' + 10)
										case 'a', 'b', 'c', 'd', 'e', 'f':
											r = (r << 4) | (chr - 'A' + 10)
										case ';':
											idx++
											return r
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
									switch chr := runes[idx]; chr {
										case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
											r = (r << 4) | (chr - '0')
										case 'A', 'B', 'C', 'D', 'E', 'F':
											r = (r << 4) | (chr - 'a' + 10)
										case 'a', 'b', 'c', 'd', 'e', 'f':
											r = (r << 4) | (chr - 'A' + 10)
										default:
											return r
									}
									idx++
								}
								if !utf8.ValidRune(r) {
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
									switch chr := runes[idx]; chr {
										case '0', '1', '2', '3', '4', '5', '6', '7':
											r = (r << 3) | (chr - '0')
										case ';':
											idx++
											return r
										default:
											return r
									}
									idx++
								}
								if !utf8.ValidRune(r) {
									return -1
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
			var kind TokenKind
			if q=='"' {
				kind = TKStrLit
			} else {
				kind = TKCharLit
			}
			tokens = append(tokens, Token{Lexeme: b.String(), Path: &filename, Line: starting_line, Col: col, Kind: kind})
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
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: kind})
				continue
			} else {
				writeMsg(nil, os.Stdout, filename, "lex error", COLOR_RED, &line, &col, "unknown operator: '%c'.", runes[starting])
				goto errored_return
			}
		}
	}
errored_return:
	tokens = append(tokens, Token{Lexeme: "<eof>", Path: &filename, Line: line, Col: idx - start, Kind: TKEoF})
	return tokens
}

func ConcatStringLiterals(tokens []Token) []Token {
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		if tokens[i].Kind==TKStrLit && i + 1 < num_tokens && tokens[i+1].Kind==TKEllipses && i + 2 < num_tokens && tokens[i+2].Kind==TKStrLit {
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

func StripNewlineTokens(tokens []Token) []Token {
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		if tokens[i].Kind==TKNewline {
			tokens = append(tokens[:i], tokens[i+1:]...)
			num_tokens = len(tokens)
			i = 0
		}
	}
	return tokens
}

func RemoveComments(tokens []Token) []Token {
	num_tokens := len(tokens)
	for i:=0; i < num_tokens; i++ {
		if tokens[i].Kind==TKComment {
			tokens = append(tokens[:i], tokens[i+1:]...)
			num_tokens = len(tokens)
			i = 0
		}
	}
	return tokens
}