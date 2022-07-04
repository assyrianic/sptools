package SPTools

import (
	"fmt"
	"os"
	"io"
	"io/ioutil"
	"strings"
)

// colorful strings for printing.
const (
	COLOR_RED     = "\x1B[31m"    // used for errors.
	COLOR_GREEN   = "\x1B[32m"
	COLOR_YELLOW  = "\x1B[33m"
	COLOR_BLUE    = "\x1B[34m"
	COLOR_MAGENTA = "\x1B[35m"    // used for warnings.
	COLOR_CYAN    = "\x1B[36m"
	COLOR_WHITE   = "\x1B[37m"
	COLOR_RESET   = "\033[0m"     // used to reset the color.
)


func makeMsg(filename, msgtype, color string, line, col *uint32, msg_fmt string, args ...any) string {
	var sb strings.Builder
	if filename != "" {
		sb.WriteString("(")
		sb.WriteString(filename)
		if line != nil {
			sb.WriteString(fmt.Sprintf(":%d", *line))
		}
		if col != nil {
			sb.WriteString(fmt.Sprintf(":%d", *col))
		}
		sb.WriteString(") ")
	}
	sb.WriteString(fmt.Sprintf("%s%s%s: **** ", color, msgtype, COLOR_RESET))
	sb.WriteString(fmt.Sprintf(msg_fmt, args...))
	sb.WriteString(" ****")
	return sb.String()
}

// prints out a message like: "(filename:line:col) msgtype: **** msg_fmt ****" to io.Writer
// 'msg_cnt, line, col' can be nil
func writeMsg(msg_cnt *uint32, w io.Writer, filename, msgtype, color string, line, col *uint32, msg_fmt string, args ...any) {
	fmt.Fprintf(w, "%s\n", makeMsg(filename, msgtype, color, line, col, msg_fmt, args...))
	if msg_cnt != nil {
		*msg_cnt++
	}
}

func loadFile(filename string) (string, string) {
	if text, read_err := ioutil.ReadFile(filename); read_err==nil {
		return string(text), "none"
	} else {
		return "", read_err.Error()
	}
}


const (
	// Runs preprocessor.
	LEXFLAG_PREPROCESS     = (1 << iota)
	
	// Self explanatory, strips out all comment tokens.
	LEXFLAG_STRIP_COMMENTS = (1 << iota)
	
	// Keeps newlines.
	LEXFLAG_NEWLINES       = (1 << iota)
	
	// Adds #include <sourcemod> automatically.
	LEXFLAG_SM_INCLUDE     = (1 << iota)
	
	// Enable ALL the above flags.
	LEXFLAG_ALL            = -1
)

// Lexes and preprocesses a file, returning its token array.
func LexFile(filename string, flags int, macros map[string]Macro) ([]Token, bool) {
	code, err_str := loadFile(filename)
	if len(code) <= 0 {
		writeMsg(nil, os.Stdout, "sptools", "IO error", COLOR_RED, nil, nil, "file error:: '%s'.", err_str)
		return []Token{}, false
	}
	if flags & LEXFLAG_SM_INCLUDE > 0 {
		code = "#include <sourcemod>\n" + code
	}
	return finishLexing(Tokenize(code, filename), flags, macros)
}

func LexCodeString(code string, flags int, macros map[string]Macro) ([]Token, bool) {
	return finishLexing(Tokenize(code, ""), flags, macros)
}

func finishLexing(tokens []Token, flags int, macros map[string]Macro) ([]Token, bool) {
	if flags & LEXFLAG_PREPROCESS > 0 {
		if output, res := Preprocess(tokens, flags, macros); res {
			tokens = output
		} else {
			return tokens, false
		}
	}
	tokens = ConcatStringLiterals(tokens)
	if flags & LEXFLAG_STRIP_COMMENTS > 0 {
		tokens = RemoveComments(tokens)
	}
	tokens = StripSpaceTokens(tokens, flags & LEXFLAG_NEWLINES > 0)
	return tokens, true
}

func MakeParser(tokens []Token) Parser {
	return Parser{ TokenReader: MakeTokenReader(tokens) }
}


func ParseTokens(tokens []Token) Node {
	parser := MakeParser(tokens)
	return parser.Start()
}


func ParseFile(filename string, flags int, macros map[string]Macro) Node {
	if tokens, result := LexFile(filename, flags, macros); !result {
		return nil
	} else {
		return ParseTokens(tokens)
	}
}

func ParseString(code string, flags int, macros map[string]Macro) Node {
	output, good := finishLexing(Tokenize(code, ""), flags, macros)
	if good {
		return ParseTokens(output)
	}
	return nil
}

func ParseExpression(code string, flags int, macros map[string]Macro) Expr {
	output, good := finishLexing(Tokenize(code, ""), flags, macros)
	if good {
		parser := MakeParser(output)
		return parser.MainExpr()
	}
	return nil
}

func ParseStatement(code string, flags int, macros map[string]Macro) Stmt {
	output, good := finishLexing(Tokenize(code, ""), flags, macros)
	if good {
		parser := Parser{ TokenReader: MakeTokenReader(output) }
		return parser.Statement()
	}
	return nil
}


func Ternary[T any](cond bool, a, b T) T {
	if cond {
		return a
	}
	return b
}