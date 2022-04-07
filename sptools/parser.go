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


func copyPosToNode(n *node, t Token) {
	n.pos.Line, n.pos.Col = t.Line, t.Col
}

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
		// continue on and try to parse the remainder
		parser.idx++
	}
}


func (parser *Parser) Start() Node {
	return parser.PrefixExpr()
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
	BoolLit
)

type (
	Expr interface {
		Node
		aExpr()
	}
	
	BadExpr struct {
		expr
	}
	
	ViewAsExpr struct {
		Type TypeExpr
		X Expr
		expr
	}
	
	TypeExpr struct {
		Tok Token
		expr
	}
	
	// ++i, i++ sizeof new
	UnaryExpr struct {
		X Expr
		Kind TokenKind
		Post bool
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
	
	// null
	NullExpr struct {
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


// Prefix = ( *( '!' | '~' | '-' | '++' | '--' ) ) | ('sizeof' | 'defined' | 'new') Postfix .
func (parser *Parser) PrefixExpr() Expr {
	// certain patterns are allowed to recursively run Prefix.
	switch t := parser.tokens[parser.idx]; t.Kind {
		case TKIncr, TKDecr, TKNot, TKCompl, TKSub, TKSizeof, TKDefined, TKNew:
			n := new(UnaryExpr)
			parser.idx++
			copyPosToNode(&n.node, t)
			n.X = parser.PrefixExpr()
			n.Kind = t.Kind
			return n
		default:
			return parser.PostfixExpr()
	}
}


// TODO: do view_as<type>(expr), also allow doing functional type casting: int(expr)

// TypeExpr = '<' ( ident | '[u]int[8|16|32|64|n]' | 'float' | 'char' | 'bool' ) '>'
func (parser *Parser) TypeExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	ret_expr := Expr(nil)
	parser.want(TKLess, "<")
	if t := parser.tokens[parser.idx]; t.IsType() || t.Kind==TKIdent {
		texp := new(TypeExpr)
		copyPosToNode(&texp.node, t)
		texp.Tok = t
		ret_expr = texp
	} else {
		parser.syntaxErr("view_as missing type expression")
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, t)
		ret_expr = err_expr
	}
	parser.want(TKGreater, ">")
	return ret_expr
}

// Postfix = Primary *( '.' identifier | '[' Expr ']' | '(' [ ExprList ] ')' | '::' identifier | '++' | '--' ) .
func (parser *Parser) PostfixExpr() Expr {
	n := parser.PrimaryExpr()
	if parser.idx >= len(parser.tokens) {
		return n
	}
	
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && (t.Kind==TKDot || t.Kind==TKLBrack || t.Kind==TKLParen || t.Kind==TK2Colons || t.Kind==TKIncr || t.Kind==TKDecr); t = parser.tokens[parser.idx] {
		parser.idx++
		switch t.Kind {
			case TKDot:
				field := new(FieldExpr)
				copyPosToNode(&field.node, t)
				field.X = n
				field.Sel = parser.PrimaryExpr()
				n = field
			case TK2Colons:
				namespc := new(NameSpaceExpr)
				copyPosToNode(&namespc.node, t)
				namespc.N = n
				namespc.Id = parser.PrimaryExpr()
				n = namespc
			case TKLBrack:
				arr := new(IndexExpr)
				copyPosToNode(&arr.node, t)
				arr.X = n
				arr.Index = parser.PrimaryExpr()
				parser.want(TKRBrack, "]")
				n = arr
			case TKIncr, TKDecr:
				incr := new(UnaryExpr)
				copyPosToNode(&incr.node, t)
				incr.X = n
				incr.Kind = t.Kind
				incr.Post = true
				n = incr
			case TKLParen:
				call := new(CallExpr)
				copyPosToNode(&call.node, t)
				call.Func = n
				func() {
					for parser.idx < len(parser.tokens) && parser.tokens[parser.idx].Kind != TKRParen {
						if len(call.ArgList) > 0 {
							parser.want(TKComma, ",")
						}
						call.ArgList = append(call.ArgList, parser.PrefixExpr())
					}
				}()
				parser.want(TKRParen, ")")
				n = call
		}
	}
	return n
}

// Primary = int_lit | rune_lit | string_lit | identifier | 'true' | 'false' | 'this' | 'null' | '(' Expr ')' .
func (parser *Parser) PrimaryExpr() Expr {
	ret_expr := Expr(nil)
	if parser.idx >= len(parser.tokens) || parser.tokens[parser.idx].Kind==TKEoF {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	switch prim := parser.tokens[parser.idx]; prim.Kind {
		case TKLParen:
			parser.idx++
			ret_expr = parser.PrefixExpr()
			parser.want(TKRParen, ")")
		case TKIdent:
			iden := new(Name)
			iden.Value = prim.Lexeme
			copyPosToNode(&iden.node, prim)
			ret_expr = iden
		case TKIntLit:
			num := new(BasicLit)
			num.Value = prim.Lexeme
			num.Kind = IntLit
			copyPosToNode(&num.node, prim)
			ret_expr = num
		case TKFloatLit:
			num := new(BasicLit)
			num.Value = prim.Lexeme
			num.Kind = FloatLit
			copyPosToNode(&num.node, prim)
			ret_expr = num
		case TKStrLit:
			str := new(BasicLit)
			str.Value = prim.Lexeme
			str.Kind = StringLit
			copyPosToNode(&str.node, prim)
			ret_expr = str
		case TKCharLit:
			str := new(BasicLit)
			str.Value = prim.Lexeme
			str.Kind = CharLit
			copyPosToNode(&str.node, prim)
			ret_expr = str
		case TKThis:
			this := new(ThisExpr)
			copyPosToNode(&this.node, prim)
			ret_expr = this
		case TKTrue, TKFalse:
			boolean := new(BasicLit)
			boolean.Value = prim.Lexeme
			boolean.Kind = BoolLit
			copyPosToNode(&boolean.node, prim)
			ret_expr = boolean
		case TKNull:
			null := new(NullExpr)
			copyPosToNode(&null.node, prim)
			ret_expr = null
		default:
			parser.syntaxErr("bad primary expression")
			err_expr := new(BadExpr)
			copyPosToNode(&err_expr.node, prim)
			ret_expr = err_expr
	}
	parser.idx++
	return ret_expr
}


func Walk(n Node, visitor func(Node) bool) {
	if !visitor(n) {
		return
	}
	
	switch ast := n.(type) {
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
		case *UnaryExpr:
			Walk(ast.X, visitor)
	}
}