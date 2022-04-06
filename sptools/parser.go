package SPTools

import (
	"os"
)


type (
	Pos struct {
		Line, Col int
	}
	Node interface {
		// line & col.
		Pos() Pos
		aNode()
	}
)

type node struct {
	pos Pos
}
func (n *node) Pos() Pos { return n.pos }
func (*node) aNode()       {}


type Parser struct {
	tokens  []Token
	idx, Errs int
}

func (parser *Parser) got(tk TokenKind) bool {
	token := parser.tokens[parser.idx];
	if token.Kind==tk {
		parser.idx++
		return true
	}
	return false
}

func (parser *Parser) syntaxErr(msg string, args ...interface{}) {
	token := parser.tokens[parser.idx]
	writeMsg(&parser.Errs, os.Stdout, *token.Path, "syntax error", COLOR_RED, &token.Line, &token.Col, msg, args...)
}

func (parser *Parser) want(tk TokenKind, lexeme string) {
	if !parser.got(tk) {
		parser.syntaxErr("expecting '%s'", lexeme)
	}
}


func (parser *Parser) Start() Node {
	return parser.PostfixExpr()
}


func (parser *Parser) sepList() {
}


// expression nodes here.
// Expression syntax write here.
type LitKind uint8
const (
	IntLit LitKind = iota
	FloatLit
	CharLit
	StringLit
)

type (
	Expr interface {
		Node
		aExpr()
	}
	
	BadExpr struct {
		expr
	}
	
	// id.name
	FieldExpr struct {
		X, Sel Expr
		expr
	}
	
	// name::id
	NameSpaceExpr struct {
		N, Id Expr
		expr
	}
	
	// a[i]
	IndexExpr struct {
		X, Index Expr
		expr
	}
	
	// f(a,b,...z);
	CallExpr struct {
		ArgList []Expr // nil means no arguments
		Func      Expr
		expr
	}
	
	// this.XXX
	ThisExpr struct {
		expr
	}
	
	// i
	Name struct {
		Value string
		expr
	}
	
	// 1, 1.0, '1', "1"
	BasicLit struct {
		Value string
		Kind  LitKind
		expr
	}
)
type expr struct{ node }
func (*expr) aExpr() {}


// Prefix = ( 'sizeof' | 'defined' | 'new' | '!' | '~' | '-' | '++' | '--' ) PostFix
func (parser *Parser) PrefixExpr() Expr {
	return nil
}

// PostFix = Primary *( '.' identifier | '[' Expr ']' | '(' [ ExprList ] ')' | '::' identifier ) .
func (parser *Parser) PostfixExpr() Expr {
	primary := parser.PrimaryExpr()
	for parser.idx < len(parser.tokens) && (parser.tokens[parser.idx].Kind==TKDot || parser.tokens[parser.idx].Kind==TKLBrack || parser.tokens[parser.idx].Kind==TKLParen || parser.tokens[parser.idx].Kind==TK2Colons) {
		t := parser.tokens[parser.idx]
		parser.idx++
		switch t.Kind {
			case TKDot:
				field := new(FieldExpr)
				field.X = primary
				field.Sel = parser.PrimaryExpr()
				primary = field
			case TK2Colons:
				namespc := new(NameSpaceExpr)
				namespc.N = primary
				namespc.Id = parser.PrimaryExpr()
				primary = namespc
			case TKLBrack:
				arr := new(IndexExpr)
				arr.X = primary
				arr.Index = parser.PrimaryExpr()
				parser.want(TKRBrack, "]")
				primary = arr
			case TKLParen:
				call := new(CallExpr)
				call.Func = primary
				func() {
					for parser.tokens[parser.idx].Kind != TKRParen {
						if len(call.ArgList) > 0 {
							parser.want(TKComma, ",")
						}
						call.ArgList = append(call.ArgList, parser.PostfixExpr())
					}
				}()
				parser.want(TKRParen, ")")
				primary = call
		}
	}
	return primary
}

// Primary = int_lit | rune_lit | string_lit | identifier | 'this' | '(' Expr ')' .
func (parser *Parser) PrimaryExpr() Expr {
	ret_expr := Expr(nil)
	if parser.idx >= len(parser.tokens) || parser.tokens[parser.idx].Kind==TKEoF {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		err_expr.node.pos.Line, err_expr.node.pos.Col = parser.tokens[i-1].Line, parser.tokens[i-1].Col
		return err_expr
	}
	switch prim := parser.tokens[parser.idx]; prim.Kind {
		//case TKLParen:
			// nested expression
		case TKIdent:
			iden := new(Name)
			iden.Value = prim.Lexeme
			iden.node.pos = Pos{ Line: prim.Line, Col: prim.Col }
			ret_expr = iden
		case TKIntLit:
			num := new(BasicLit)
			num.Value = prim.Lexeme
			num.Kind = IntLit
			num.node.pos = Pos{ Line: prim.Line, Col: prim.Col }
			ret_expr = num
		case TKFloatLit:
			num := new(BasicLit)
			num.Value = prim.Lexeme
			num.Kind = FloatLit
			num.node.pos = Pos{ Line: prim.Line, Col: prim.Col }
			ret_expr = num
		case TKStrLit:
			str := new(BasicLit)
			str.Value = prim.Lexeme
			str.Kind = StringLit
			str.node.pos = Pos{ Line: prim.Line, Col: prim.Col }
			ret_expr = str
		case TKCharLit:
			str := new(BasicLit)
			str.Value = prim.Lexeme
			str.Kind = CharLit
			str.node.pos = Pos{ Line: prim.Line, Col: prim.Col }
			ret_expr = str
		case TKThis:
			this := new(ThisExpr)
			this.node.pos = Pos{ Line: prim.Line, Col: prim.Col }
			ret_expr = this
		default:
			parser.syntaxErr("bad primary expression")
			err_expr := new(BadExpr)
			err_expr.node.pos.Line, err_expr.node.pos.Col = prim.Line, prim.Col
			ret_expr = err_expr
	}
	parser.idx++
	return ret_expr
}


func Walk(node Node, visitor func(Node) bool) {
	if !visitor(node) {
		return
	}
	
	switch ast := node.(type) {
		//case *BasicLit:
		//case *Name:
		//case *ThisExpr:
		case *CallExpr:
			Walk(ast.Func, visitor)
			if ast.ArgList != nil {
				for i := range ast.ArgList {
					Walk(ast.ArgList[i], visitor)
				}
			}
		case *IndexExpr:
			Walk(ast.X, visitor)
			Walk(ast.Index, visitor)
		case *NameSpaceExpr:
			Walk(ast.N, visitor)
			Walk(ast.Id, visitor)
		case *FieldExpr:
			Walk(ast.X, visitor)
			Walk(ast.Sel, visitor)
	}
}