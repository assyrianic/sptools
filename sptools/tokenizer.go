package SPTools

import (
	"strings"
	"unicode"
	"fmt"
	"os"
	///"time"
	"unicode/utf8"
)


// DigitSep is used when lexing the various numeric literals
// SourcePawn can support such as hexadecimal, octal, binary, & decimal.
const DigitSep = '_'

func isIden(c rune) bool {
	return unicode.IsLetter(c) || unicode.IsNumber(c) || c==DigitSep || c=='$'
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

// Represents a token's kind/class.
type TokenKind uint16
const (
	TKEoF = TokenKind(iota)
	TKComment
	
	// used for preprocessor, removed afterwards.
	TKNewline
	TKSpace
	TKTab
	TKBackSlash
	TKHashTok
	TKMacroArg
	
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
	
	// package, private, property, protected, public
	TKPackage
	TKPrivate
	TKProperty
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
	// #assert #define #if #else #elseif #endif #endinput #endscript #error #warning #include #line #pragma #tryinclude #undef
	TKPPAssert
	TKPPDefine
	TKPPIf
	TKPPElse
	TKPPElseIf
	TKPPEndIf
	TKPPEndInput
	TKPPEndScript
	TKPPErr
	TKPPWarn
	TKPPInclude
	TKPPLine
	TKPPPragma
	TKPPTryInclude
	TKPPUndef
	
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
		"property": TKProperty,
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
		"typeset": TKTypeset,
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
		"#assert": TKPPAssert,
		"#define": TKPPDefine,
		"#if": TKPPIf,
		"#else": TKPPElse,
		"#elif": TKPPElseIf,
		"#endif": TKPPEndIf,
		"#endinput": TKPPEndInput,
		"#endscript": TKPPEndScript,
		"#error": TKPPErr,
		"#warning": TKPPWarn,
		"#include": TKPPInclude,
		"#line": TKPPLine,
		"#pragma": TKPPPragma,
		"#tryinclude": TKPPTryInclude,
		"#undef": TKPPUndef,
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
		TKSpace: "<space>",
		TKTab: "<tab>",
		TKBackSlash: "<\\\\>",
		TKHashTok: "<#%0>",
		TKMacroArg: "<%0>",
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
		TKProperty: "property",
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
		TKTypeset: "typeset",
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
		
		// preprocessor tokens
		TKPPAssert: "#assert",
		TKPPDefine: "#define",
		TKPPElse: "#else",
		TKPPElseIf: "#elif",
		TKPPEndIf: "#endif",
		TKPPEndInput: "#endinput",
		TKPPEndScript: "#endscript",
		TKPPErr: "#error",
		TKPPWarn: "#warning",
		TKPPIf: "#if",
		TKPPInclude: "#include",
		TKPPLine: "#line",
		TKPPPragma: "#pragma",
		TKPPTryInclude: "#tryinclude",
		TKPPUndef: "#undef",
		TKHash: "#",
		
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
	Line, Col uint32
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

func (tok Token) IsPreprocDirective() bool {
	return tok.Kind >= TKPPAssert && tok.Kind <= TKPPUndef
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

// returns either the lexeme or operator string.
func (tok Token) String() string {
	if tok.Kind >= TKComment && tok.Kind <= TKCharLit {
		return tok.Lexeme
	} else {
		return TokenToStr[tok.Kind]
	}
}

func (tok Token) ToString() string {
	return fmt.Sprintf("Token:: %q - line: '%d', col: '%d' | file: %q", tok.String(), tok.Line, tok.Col, *tok.Path)
}


type TokenReader struct {
	Tokens []Token
	Idx      int
}

func MakeTokenReader(tokens []Token) TokenReader {
	return TokenReader{ Tokens: tokens }
}

func (tr *TokenReader) Get(offset int, ignore_ws, ignore_comments bool) Token {
	index := tr.Idx + offset
	if tlen := tr.Len(); index >= tlen || index < 0 {
		return tr.Tokens[tlen - 1]
	}
	
	t := tr.Tokens[index]
	if (ignore_ws && (t.Kind==TKNewline || t.Kind==TKSpace || t.Kind==TKTab)) || (ignore_comments && t.Kind==TKComment) {
		tr.Advance(1)
		return tr.Get(offset, ignore_ws, ignore_comments)
	}
	return t
}

func (tr *TokenReader) Advance(i int) {
	if i < 1 {
		i = 1 // clamp min value.
	}
	tr.Idx += i
}

func (tr *TokenReader) HasTokenKindSeq(ignore_ws, ignore_comments bool, kinds ...TokenKind) bool {
	matched := true
	for i := range kinds {
		if tr.Get(i, ignore_ws, ignore_comments).Kind != kinds[i] {
			matched = false
			break
		}
	}
	return matched
}

func (tr *TokenReader) Reset() {
	tr.Idx = 0
}

func (tr *TokenReader) Len() int {
	return len(tr.Tokens)
}

func (tr *TokenReader) SkipTokenKinds(kinds ...TokenKind) {
	for t := tr.Get(0, false, false); t.Kind != TKEoF; t = tr.Get(0, false, false) {
		got_something := false
		for i := range kinds {
			if t.Kind==kinds[i] {
				tr.Advance(1)
				got_something = true
				break
			}
		}
		if !got_something {
			break
		}
	}
}


type Scanner struct {
	runes       []rune
	filename      string
	idx, start    int
	line, numMsgs uint32
}

func (s Scanner) Read(i int) rune {
	if l := len(s.runes); s.idx + i >= l {
		return 0
	} else {
		return s.runes[s.idx + i]
	}
}

func (s *Scanner) Advance(i int) {
	s.idx += i
}

func (s Scanner) NumMsgs() uint32 {
	return s.numMsgs
}

func (s Scanner) Line() uint32 {
	return s.line
}

func (s Scanner) Col() uint32 {
	return uint32(s.idx - s.start)
}

func (s Scanner) HasRuneSeq(runes ...rune) bool {
	matched := true
	for i := range runes {
		if s.Read(i) != runes[i] {
			matched = false
			break
		}
	}
	return matched
}

func (s *Scanner) SkipSpace() {
	for chr := s.Read(0); chr != 0 && (chr==' ' || chr=='\t' || chr=='\r'); chr = s.Read(0) {
		s.idx++
	}
}

func (s *Scanner) stripCarriageReturn() {
	i, limit := 0, len(s.runes)
	for i < limit {
		if s.runes[i]=='\r' {
			s.runes = append(s.runes[:i], s.runes[i+1:]...)
			limit = len(s.runes)
			i = 0
			continue
		}
		i++
	}
}


func (s *Scanner) LexBinary() (string, bool) {
	if s.Read(0) != '0' || (s.Read(1) != 'b' && s.Read(1) != 'B') {
		return "", false
	}
	start := s.idx
	s.idx += 2
	for chr := s.Read(0); isAlphaNum(chr) || chr==DigitSep || chr=='-'; chr = s.Read(0) {
		switch chr {
			case '0', '1', DigitSep:
				s.idx++
			case '-':
				if !unicode.IsNumber(s.Read(1)) {
					return string(s.runes[start : s.idx]), false
				}
				s.idx++
			default:
				col := s.Col()
				s.idx = start
				writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "bad digit %c in binary literal", chr)
				return "", false
		}
	}
	return string(s.runes[start : s.idx]), true
}

func (s *Scanner) LexHex() (string, bool) {
	if s.Read(0) != '0' || (s.Read(1) != 'x' && s.Read(1) != 'X') {
		return "", false
	}
	start := s.idx
	s.idx += 2
	for chr := s.Read(0); isAlphaNum(chr) || chr==DigitSep || chr=='-'; chr = s.Read(0) {
		switch chr {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				fallthrough
			case 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F':
				s.idx++
			case '-':
				if !unicode.IsNumber(s.Read(1)) {
					return string(s.runes[start : s.idx]), false
				}
				s.idx++
			case DigitSep:
				if look := s.Read(1); !unicode.IsNumber(look) && !isHex(look) {
					return string(s.runes[start : s.idx]), false
				}
				s.idx++
			default:
				col := s.Col()
				s.idx = start
				writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "bad digit %c in hex literal", chr)
				return "", false
		}
	}
	return string(s.runes[start : s.idx]), true
}

func (s *Scanner) LexOctal() (string, bool) {
	if s.Read(0) != '0' || (s.Read(1) != 'o' && s.Read(1) != 'O') {
		return "", false
	}
	start := s.idx
	s.idx += 2
	for chr := s.Read(0); isAlphaNum(chr) || chr==DigitSep; chr = s.Read(0) {
		switch chr {
			case '0', '1', '2', '3', '4', '5', '6', '7':
				s.idx++
			case DigitSep:
				if look := s.Read(1); !isOctal(look) {
					return string(s.runes[start : s.idx]), false
				}
				s.idx++
			default:
				col := s.Col()
				s.idx = start
				writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "bad digit %c in octal literal", chr)
				return "", false
		}
	}
	return string(s.runes[start : s.idx]), true
}

// returns the lexeme, result, and if it's a float.
func (s *Scanner) LexDecimal() (string, bool, bool) {
	if s.Read(0)==0 {
		return "", false, false
	}
	start := s.idx
	var got_num bool
	for chr := s.Read(0); isAlphaNum(chr) || chr==DigitSep || chr=='.' || chr=='-'; chr = s.Read(0) {
		switch chr {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				if !got_num {
					got_num = true
				}
				s.idx++
			case DigitSep:
				if look := s.Read(1); !unicode.IsNumber(look) {
					return string(s.runes[start : s.idx]), true, false
				}
				s.idx++
			case '-':
				if !unicode.IsNumber(s.Read(1)) {
					return string(s.runes[start : s.idx]), true, false
				}
				s.idx++
			case '.':
				if s.HasRuneSeq('.', '.', '.') {
					return string(s.runes[start : s.idx]), true, false
				}
				return s.LexFloat(start, got_num)
			default:
				col := s.Col()
				s.idx = start
				writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "bad digit %c in decimal literal", chr)
				return "", false, false
		}
	}
	return string(s.runes[start : s.idx]), true, false
}

func (s *Scanner) LexFloat(starter int, has_num bool) (string, bool, bool) {
	if s.Read(0)==0 {
		return "", false, true
	}
	s.idx = starter
	start, got_num := s.idx, has_num
	var got_E, num_after_E, got_math bool
	for chr := s.Read(0); (isAlphaNum(chr) || chr==DigitSep || chr=='.' || chr=='+' || chr=='-'); chr = s.Read(0) {
		switch chr {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				if !got_num {
					got_num = true
				}
				if got_E && !num_after_E {
					num_after_E = true
				}
				s.idx++
			case DigitSep:
				if look := s.Read(1); !unicode.IsNumber(look) {
					return string(s.runes[start : s.idx]), true, true
				}
				s.idx++
			case '.':
				if !got_num {
					col := s.Col()
					s.idx = start
					writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "'.' in float literal before numbers.")
					return "", false, true
				}
				s.idx++
			case 'e':
				if got_E {
					col := s.Col()
					s.idx = start
					writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "too many Es in float.")
					return "", false, true
				}
				got_E = true
				s.idx++
			case '+', '-':
				if num_after_E || got_math {
					return "", true, true
				} else if got_E && !unicode.IsDigit(s.Read(1)) {
					col := s.Col()
					s.idx = start
					writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "missing numbers after +/- in E exponent.")
					return "", false, true
				}
				got_math = true
				s.idx++
			default:
				col := s.Col()
				s.idx = start
				writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "bad digit %c in float literal.", chr)
				return "", false, true
		}
	}
	if got_E && !num_after_E {
		col := s.Col()
		s.idx = start
		writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "exponent E is missing numbers in float literal.")
		return "", false, true
	}
	return string(s.runes[start : s.idx]), true, true
}

func (s *Scanner) LexString(quote rune) (Token, bool) {
	if s.Read(0)==0 {
		return Token{}, false
	}
	col, starting_line, q := s.Col(), s.line, quote
	s.idx++
	var b strings.Builder
	for s.Read(0) != q {
		if s.Read(0)=='\\' {
			s.idx++
			switch esc := s.Read(0); esc {
				case '\n':
					s.line++
					s.start = s.idx
					s.idx++
				case '\'', '"':
					b.WriteRune(esc)
					s.idx++
				case '\\':
					b.WriteRune('\\')
					s.idx++
				case 'a':
					b.WriteRune('\a')
					s.idx++
				case 'r':
					b.WriteRune('\r')
					s.idx++
				case 'b':
					b.WriteRune('\b')
					s.idx++
				case 't':
					b.WriteRune('\t')
					s.idx++
				case 'v':
					b.WriteRune('\v')
					s.idx++
				case 'n':
					b.WriteRune('\n')
					s.idx++
				case 'f':
					b.WriteRune('\f')
					s.idx++
				case 'X', 'x':
					s.idx++
					value := func() rune {
						if !isHex(s.Read(0)) {
							return -1
						}
						var r rune
						for s.Read(0) > 0 {
							switch chr := s.Read(0); chr {
								case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
									r = (r << 4) | (chr - '0')
								case 'A', 'B', 'C', 'D', 'E', 'F':
									r = (r << 4) | (chr - 'a' + 10)
								case 'a', 'b', 'c', 'd', 'e', 'f':
									r = (r << 4) | (chr - 'A' + 10)
								case ';':
									s.idx++
									return r
								default:
									return r
							}
							s.idx++
						}
						return r
					}()
					if utf8.ValidRune(value) {
						b.WriteRune(value)
					}
				case 'u', 'U':
					s.idx++
					value := func(is_u32 bool) rune {
						var r rune
						var encoding_size int
						if is_u32 {
							encoding_size = 8
						} else {
							encoding_size = 4
						}
						for n:=0; s.Read(0) > 0 && n < encoding_size; n++ {
							switch chr := s.Read(0); chr {
								case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
									r = (r << 4) | (chr - '0')
								case 'A', 'B', 'C', 'D', 'E', 'F':
									r = (r << 4) | (chr - 'a' + 10)
								case 'a', 'b', 'c', 'd', 'e', 'f':
									r = (r << 4) | (chr - 'A' + 10)
								default:
									return r
							}
							s.idx++
						}
						return r
					}(esc=='U')
					if utf8.ValidRune(value) {
						b.WriteRune(value)
					}
				case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
					value := func() rune {
						if !isOctal(s.Read(0)) {
							return -1
						}
						var r rune
						for s.Read(0) > 0 {
							switch chr := s.Read(0); chr {
								case '0', '1', '2', '3', '4', '5', '6', '7':
									r = (r << 3) | (chr - '0')
								case ';':
									s.idx++
									return r
								default:
									return r
							}
							s.idx++
						}
						return r
					}()
					if utf8.ValidRune(value) {
						b.WriteRune(value)
					}
			}
		} else if s.Read(0)=='\n' {
			break
		} else {
			b.WriteRune(s.Read(0))
			s.idx++
		}
	}
	s.idx++
	kind := Ternary[TokenKind](q=='"', TKStrLit, TKCharLit)
	return Token{Lexeme: b.String(), Path: &s.filename, Line: starting_line, Col: col, Kind: kind}, true
}


func Tokenize(src, filename string) []Token {
	var (
		tokens []Token
		in_preproc bool
	)
	s := Scanner{
		runes: ([]rune)(src),
		filename: filename,
		line: 1,
	}
	s.stripCarriageReturn()
	for s.Read(0) > 0 {
		///fmt.Printf("Read: '%d' | '%c'\n", s.Read(0), s.Read(0))
		///time.Sleep(50 * time.Millisecond)
		if c := s.Read(0); unicode.IsSpace(c) {
			starting := s.idx
			s.idx++
			switch c {
			case '\n':
				in_preproc = false
				tokens = append(tokens, Token{Lexeme: "\n", Path: &filename, Line: s.line, Col: s.Col(), Kind: TKNewline})
				s.line++
				s.start = s.idx
			case ' ': // eat up the space tokens as much as possible.
				for s.Read(0) != 0 && s.Read(0)==c {
					s.idx++
				}
				lexeme := string(s.runes[starting : s.idx])
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: s.line, Col: s.Col(), Kind: TKSpace})
			case '\t':
				for s.Read(0) != 0 && s.Read(0)==c {
					s.idx++
				}
				lexeme := string(s.runes[starting : s.idx])
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: s.line, Col: s.Col(), Kind: TKTab})
			}
		} else if c=='\\' {
			for s.Read(0) != 0 && s.Read(0) != '\n' {
				s.idx++
			}
			if in_preproc {
				s.idx++
				s.line++
				s.start = s.idx
			}
			tokens = append(tokens, Token{Lexeme: "\\", Path: &filename, Line: s.line, Col: s.Col(), Kind: TKBackSlash})
		} else if unicode.IsLetter(c) || c=='_' {
			// handle identifiers & keywords.
			col, starting := s.Col(), s.idx
			for isIden(s.Read(0)) {
				s.idx++
			}
			lexeme := string(s.runes[starting : s.idx])
			if tkind, found := Keywords[lexeme]; found {
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: s.line, Col: col, Kind: tkind})
			} else {
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: s.line, Col: col, Kind: TKIdent})
			}
		} else if c=='/' && s.Read(1)=='/' {
			// single line comment.
			col, starting, starting_line := s.Col(), s.idx, s.line
			s.idx += 2
			for s.Read(0) != 0 && s.Read(0) != '\n' {
				if s.Read(0)=='\\' {
					s.line++
					s.idx++
					s.start = s.idx
				}
				s.idx++
			}
			lexeme := string(s.runes[starting : s.idx])
			tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKComment})
		} else if c=='/' && s.Read(1)=='*' {
			// multi-line comment.
			col, starting, starting_line := s.Col(), s.idx, s.line
			s.idx += 2
			stop := false
			for s.Read(0) != 0 && !stop {
				if s.Read(0)=='\n' {
					s.line++
					s.start = s.idx
				} else if s.Read(0)=='*' && s.Read(1)=='/' {
					s.idx++
					stop = true
				}
				s.idx++
			}
			lexeme := string(s.runes[starting : s.idx])
			tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKComment})
		} else if unicode.IsNumber(c) {
			// handle numbers.
			col, starting_line := s.Col(), s.line
			if lexeme, result := s.LexBinary(); result {
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKIntLit})
			} else if lexeme, result = s.LexHex(); result {
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKIntLit})
			} else if lexeme, result = s.LexOctal(); result {
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: TKIntLit})
			} else if nlexeme, res, is_float := s.LexDecimal(); res {
				var kind TokenKind
				if is_float {
					kind = TKFloatLit
				} else {
					kind = TKIntLit
				}
				tokens = append(tokens, Token{Lexeme: nlexeme, Path: &filename, Line: starting_line, Col: col, Kind: kind})
			} else {
				writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "failed to tokenize number.")
				goto errored_return
			}
		} else if c=='"' || c=='\'' {
			t, res := s.LexString(c)
			if res {
				tokens = append(tokens, t)
			} else {
				col := s.Col()
				writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "failed to tokenize string.")
				goto errored_return
			}
		} else if c=='#' {
			if unicode.IsLetter(s.Read(1)) {
				s.idx++
				col, starting := s.Col(), s.idx
				for isIden(s.Read(0)) {
					s.idx++
				}
				lexeme := string(s.runes[starting : s.idx])
				if kind, found := Keywords["#" + lexeme]; found {
					tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: s.line, Col: col, Kind: kind})
					in_preproc = true
					switch lexeme {
					case "error", "warning", "pragma":
						s.SkipSpace()
						starting := s.idx
						for s.Read(0) != 0 && s.Read(0) != '\n' {
							s.idx++
						}
						msg := string(s.runes[starting : s.idx])
						if len(msg)==0 {
							col := s.Col()
							writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "#%s directive is missing message argument.", lexeme)
							goto errored_return
						}
						tokens = append(tokens, Token{Lexeme: msg, Path: &filename, Line: s.line, Col: s.Col(), Kind: TKStrLit})
					/**
					case "include", "tryinclude":
						s.SkipSpace()
						starting := s.idx
						for s.Read(0) != 0 && !unicode.IsSpace(s.Read(0)) {
							s.idx++
						}
						lexeme := string(s.runes[starting : s.idx])
						tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: s.line, Col: s.Col(), Kind: TKStrLit})*/
					}
				} else {
					writeMsg(&s.numMsgs, os.Stdout, s.filename, "lex error", COLOR_RED, &s.line, &col, "unknown preprocessor directive: '%s'", "#" + lexeme)
					goto errored_return
				}
			} else if in_preproc && s.Read(1)=='%' && unicode.IsNumber(s.Read(2)) {
				s.idx += 2
				if lexeme, res, is_float := s.LexDecimal(); !res {
					col := s.Col()
					writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "failed to tokenize number for token pasting.")
					goto errored_return
				} else if is_float {
					col := s.Col()
					writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "cannot use float numeral for macro arg.")
					goto errored_return
				} else {
					tokens = append(tokens, Token{Lexeme: "#%" + lexeme, Path: &filename, Line: s.line, Col: s.Col(), Kind: TKHashTok})
				}
			}
		} else if in_preproc && c=='%' && unicode.IsNumber(s.Read(1)) {
			s.idx++
			if lexeme, res, is_float := s.LexDecimal(); !res {
				col := s.Col()
				writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "failed to tokenize number for token pasting.")
				goto errored_return
			} else if is_float {
				col := s.Col()
				writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "cannot use float numeral for macro arg.")
				goto errored_return
			} else {
				tokens = append(tokens, Token{Lexeme: "%" + lexeme, Path: &filename, Line: s.line, Col: s.Col(), Kind: TKMacroArg})
			}
		} else {
			col, starting, starting_line := s.Col(), s.idx, s.line
			oper_size, oper_key, got_match := 0, "", false
			for key := range Opers {
				// Match largest operator first.
				keylen := len(key)
				if s.Read(keylen-1)==0 {
					continue
				}
				
				if string(s.runes[starting : s.idx + keylen])==key && oper_size < keylen {
					oper_size, oper_key, got_match = keylen, key, true
				}
			}
			
			if got_match {
				s.idx += len(oper_key)
				lexeme, kind := string(s.runes[starting : s.idx]), Opers[oper_key]
				tokens = append(tokens, Token{Lexeme: lexeme, Path: &filename, Line: starting_line, Col: col, Kind: kind})
				continue
			} else {
				writeMsg(&s.numMsgs, os.Stdout, filename, "lex error", COLOR_RED, &s.line, &col, "unknown operator: '%c'.", s.runes[starting])
				goto errored_return
			}
		}
	}
errored_return:
	tokens = append(tokens, Token{Lexeme: "<eof>", Path: &filename, Line: s.line, Col: s.Col(), Kind: TKEoF})
	return tokens
}

func ConcatStringLiterals(tokens []Token) []Token {
	num_tokens := len(tokens)
	for i := 0; i < num_tokens; i++ {
		if tokens[i].Kind==TKStrLit && i + 2 < num_tokens && tokens[i+1].Kind==TKEllipses && tokens[i+2].Kind==TKStrLit {
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

func StripSpaceTokens(tokens []Token, leave_newlines bool) []Token {
	i, num_tokens := len(tokens), 0
	for i < num_tokens {
		if t := tokens[i]; (!leave_newlines && t.Kind==TKNewline) || t.Kind==TKSpace || t.Kind==TKTab {
			tokens = append(tokens[:i], tokens[i+1:]...)
			num_tokens = len(tokens)
			i = 0
			continue
		}
		i++
	}
	return tokens
}

func RemoveComments(tokens []Token) []Token {
	i, num_tokens := len(tokens), 0
	for i < num_tokens {
		if tokens[i].Kind==TKComment {
			tokens = append(tokens[:i], tokens[i+1:]...)
			num_tokens = len(tokens)
			i = 0
			continue
		}
		i++
	}
	return tokens
}