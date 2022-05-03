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
	LEXFLAG_PREPROCESS    = (1 << iota)
	LEXFLAG_STRIPCOMMENTS = (1 << iota)
	LEXFLAG_ALL           = -1
)

// Lexes and preprocesses a file, returning its token array.
func LexFile(filename string, flags int) ([]Token, bool) {
	var tokens []Token
	code, err_str := loadFile(filename)
	if len(code) <= 0 {
		writeMsg(nil, os.Stdout, "sptools", "IO error", COLOR_RED, nil, nil, "file error:: '%s'.", err_str)
		return tokens, false
	}
	
	tokens = Tokenize(code, filename)
	if flags & LEXFLAG_PREPROCESS > 0 {
		if output, res := Preprocess(tokens); res {
			output = StripNewlineTokens(output)
			tokens = output
		} else {
			return tokens, false
		}
	}
	tokens = ConcatStringLiterals(tokens)
	if flags & LEXFLAG_STRIPCOMMENTS > 0 {
		tokens = RemoveComments(tokens)
	}
	return tokens, true
}


func ParseTokens(tokens []Token) Node {
	parser := Parser{ tokens: tokens }
	return parser.Start()
}


func ParseFile(filename string, flags int) Node {
	if tokens, result := LexFile(filename, flags); !result {
		return nil
	} else {
		return ParseTokens(tokens)
	}
}