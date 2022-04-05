package SPTools

import (
	"fmt"
	"io"
	"io/ioutil"
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

// prints out a message like: "(filename:line:col) msgtype: **** msg_fmt ****" to io.Writer
// 'msg_cnt, line, col' can be nil
func writeMsg(msg_cnt *int, w io.Writer, filename, msgtype, color string, line, col *int, msg_fmt string, args ...interface{}) {
	if filename != "" {
		fmt.Fprintf(w, "(%s", filename)
		if line != nil {
			fmt.Fprintf(w, ":%d", *line)
		}
		if col != nil {
			fmt.Fprintf(w, ":%d", *col)
		}
		fmt.Fprintf(w, ") ")
	}
	fmt.Fprintf(w, "%s%s%s: **** ", color, msgtype, COLOR_RESET)
	fmt.Fprintf(w, msg_fmt, args...)
	fmt.Fprintf(w, " ****\n")
	if msg_cnt != nil {
		*msg_cnt++
	}
}

func loadFile(filename string) string {
	if text, read_err := ioutil.ReadFile(filename); read_err==nil {
		return string(text)
	}
	return ""
}


const (
	LEXFLAG_PREPROCESS    = (1 << iota)
	LEXFLAG_STRIPNEWLINES = (1 << iota)
	LEXFLAG_STRIPCOMMENTS = (1 << iota)
	LEXFLAG_ALL           = -1
)

// Lexes and preprocesses a file, returning its token array.
func LexFile(filename string, flags int) ([]SPToken, bool) {
	var tokens []SPToken
	code := loadFile(filename)
	if len(code) <= 0 {
		return tokens, false
	}
	
	tokens = Tokenize(code, filename)
	tokens = ConcatStringLiterals(tokens)
	if flags & LEXFLAG_PREPROCESS > 0 {
		if output, res := Preprocess(tokens); res {
			if flags & LEXFLAG_STRIPNEWLINES > 0 {
				output = StripNewlineTokens(output)
			}
			tokens = output
		} else {
			return tokens, false
		}
	}
	if flags & LEXFLAG_STRIPCOMMENTS > 0 {
		tokens = RemoveComments(tokens)
	}
	return tokens, true
}

/*
func ParseFile(filename string) *SPAst {
	
}
*/