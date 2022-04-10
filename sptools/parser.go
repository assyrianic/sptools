package SPTools

import (
	"os"
	"fmt"
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
	return parser.MainExpr()
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
var LitKindToStr = [...]string{
	IntLit: "int literal",
	FloatLit: "float literal",
	CharLit: "char literal",
	StringLit: "string literal",
	BoolLit: "bool literal",
}


type (
	Expr interface {
		Node
		aExpr()
	}
	
	BadExpr struct {
		expr
	}
	
	// a,b,c
	CommaExpr struct {
		Exprs []Expr
		expr
	}
	
	// a? b : c
	TernaryExpr struct {
		A, B, C Expr
		expr
	}
	
	// a # b
	BinExpr struct {
		L, R Expr
		Kind TokenKind
		expr
	}
	
	// <T>
	TypedExpr struct {
		Tok Token
		expr
	}
	
	// view_as<T>(expr)
	ViewAsExpr struct {
		Type Expr // *TypedExpr
		X Expr
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
	
	// .a = expr
	NamedArg struct {
		Param, X Expr
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


// Expr = SubMainExpr *( ',' SubMainExpr ) .
func (parser *Parser) MainExpr() Expr {
	a := parser.SubMainExpr()
	if parser.idx < len(parser.tokens) && parser.tokens[parser.idx].Kind==TKComma {
		c := new(CommaExpr)
		copyPosToNode(&c.node, parser.tokens[parser.idx])
		c.Exprs = append(c.Exprs, a)
		for parser.idx < len(parser.tokens) && parser.tokens[parser.idx].Kind==TKComma {
			parser.idx++
			c.Exprs = append(c.Exprs, parser.SubMainExpr())
		}
		a = c
	}
	return a
}

// SubMainExpr = LogicalOrExpr [ '?' LogicalOrExpr ':' Expr ] .
func (parser *Parser) SubMainExpr() Expr {
	a := parser.LogicalOrExpr()
	if parser.idx < len(parser.tokens) && parser.tokens[parser.idx].Kind==TKQMark {
		// ternary
		a = parser.DoTernary(a)
	}
	return a
}

func (parser *Parser) DoTernary(a Expr) Expr {
	tk := parser.tokens[parser.idx]
	t := new(TernaryExpr)
	copyPosToNode(&t.node, tk)
	t.A = a
	parser.idx++
	t.B = parser.LogicalOrExpr()
	parser.want(TKColon, ":")
	t.C = parser.MainExpr()
	return t
}

// LogicalOrExpr = LogicalAndExpr *( '||' LogicalAndExpr ) .
func (parser *Parser) LogicalOrExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.LogicalAndExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && t.Kind==TKOrL; t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.LogicalAndExpr()
		e = b
	}
	return e
}

// LogicalAndExpr = EqualExpr *( '&&' EqualExpr ) .
func (parser *Parser) LogicalAndExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.EqualExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && t.Kind==TKAndL; t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.EqualExpr()
		e = b
	}
	return e
}

// EqualExpr = RelExpr *( ( '==' | '!=' ) RelExpr ) .
func (parser *Parser) EqualExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.RelExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && (t.Kind==TKEq || t.Kind==TKNotEq); t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.RelExpr()
		e = b
	}
	return e
}

// RelExpr = BitOrExpr *( ( '<[=]' | (>[=]) ) BitOrExpr ) .
func (parser *Parser) RelExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.BitOrExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && (t.Kind>=TKLess && t.Kind<=TKLessE); t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.BitOrExpr()
		e = b
	}
	return e
}


// BitOrExpr = BitXorExpr *( '|' BitXorExpr ) .
func (parser *Parser) BitOrExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.BitXorExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && t.Kind==TKOr; t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.BitXorExpr()
		e = b
	}
	return e
}

// BitXorExpr = BitAndExpr *( '^' BitAndExpr ) .
func (parser *Parser) BitXorExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.BitAndExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && t.Kind==TKXor; t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.BitAndExpr()
		e = b
	}
	return e
}

// BitAndExpr = ShiftExpr *( '&' ShiftExpr ) .
func (parser *Parser) BitAndExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.ShiftExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && t.Kind==TKAnd; t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.ShiftExpr()
		e = b
	}
	return e
}

// ShiftExpr = AddExpr *( ( '<<' | '>>' | '>>>' ) AddExpr ) .
func (parser *Parser) ShiftExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.AddExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && (t.Kind>=TKShAL && t.Kind<=TKShLR); t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.AddExpr()
		e = b
	}
	return e
}

// AddExpr = MulExpr *( ( '+' | '-' ) MulExpr ) .
func (parser *Parser) AddExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.MulExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && (t.Kind==TKAdd || t.Kind==TKSub); t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.MulExpr()
		e = b
	}
	return e
}

// MulExpr = PrefixExpr *( ( '*' | '/' | '%' ) PrefixExpr ) .
func (parser *Parser) MulExpr() Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	e := parser.PrefixExpr()
	for t := parser.tokens[parser.idx]; parser.idx < len(parser.tokens) && (t.Kind==TKMul || t.Kind==TKDiv || t.Kind==TKMod); t = parser.tokens[parser.idx] {
		b := new(BinExpr)
		copyPosToNode(&b.node, t)
		b.L = e
		b.Kind = t.Kind
		parser.idx++
		b.R = parser.PrefixExpr()
		e = b
	}
	return e
}


// Prefix = *( '!' | '~' | '-' | '++' | '--' | 'sizeof' | 'defined' | 'new' ) Postfix .
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


// TypeExpr = '<' ( ident | '[u]int[8|16|32|64|n]' | 'float' | 'char' | 'bool' ) '>' .
func (parser *Parser) TypeExpr(need_carots bool) Expr {
	if parser.idx >= len(parser.tokens) {
		i := len(parser.tokens)
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, parser.tokens[i-1])
		return err_expr
	}
	
	ret_expr := Expr(nil)
	if need_carots {
		parser.want(TKLess, "<")
	}
	if t := parser.tokens[parser.idx]; t.IsType() || t.Kind==TKIdent {
		texp := new(TypedExpr)
		copyPosToNode(&texp.node, t)
		texp.Tok = t
		ret_expr = texp
		parser.idx++
	} else {
		parser.syntaxErr("view_as missing type expression")
		err_expr := new(BadExpr)
		copyPosToNode(&err_expr.node, t)
		ret_expr = err_expr
	}
	if need_carots {
		parser.want(TKGreater, ">")
	}
	return ret_expr
}

// ViewAsExpr = TypeExpr '(' MainExpr ')' .
func (parser *Parser) ViewAsExpr() Expr {
	view_as := new(ViewAsExpr)
	parser.want(TKViewAs, "view_as")
	copyPosToNode(&view_as.node, parser.tokens[parser.idx - 1])
	view_as.Type = parser.TypeExpr(true)
	parser.want(TKLParen, "(")
	view_as.X = parser.MainExpr()
	parser.want(TKRParen, ")")
	return view_as
}

// ExprList = Expr *( ',' Expr ) .
// Postfix = Primary *( '.' identifier | '[' Expr ']' | '(' [ ExprList ] ')' | '::' identifier | '++' | '--' ) .
func (parser *Parser) PostfixExpr() Expr {
	n := Expr(nil)
	if t := parser.tokens[parser.idx]; t.Kind==TKViewAs {
		n = parser.ViewAsExpr()
	} else {
		n = parser.PrimaryExpr()
	}
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
				arr.Index = parser.MainExpr()
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
				for parser.idx < len(parser.tokens) && parser.tokens[parser.idx].Kind != TKRParen {
					if len(call.ArgList) > 0 {
						parser.want(TKComma, ",")
					}
					// SP allows setting your params by name.
					// '.param_name = expression'
					// '.' Name '=' Expr
					if parser.tokens[parser.idx].Kind==TKDot {
						parser.idx++
						named_arg := new(NamedArg)
						copyPosToNode(&named_arg.node, parser.tokens[parser.idx-1])
						if iden := parser.tokens[parser.idx]; iden.Kind != TKIdent {
							parser.syntaxErr("expected identifier for named arg.")
						}
						named_arg.Param = parser.PrimaryExpr()
						parser.want(TKAssign, "=")
						named_arg.X = parser.SubMainExpr()
						call.ArgList = append(call.ArgList, named_arg)
					} else {
						call.ArgList = append(call.ArgList, parser.SubMainExpr())
					}
				}
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
			ret_expr = parser.MainExpr()
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

func PrintNode(n Node, tabs int) {
	for i:=0; i < tabs; i++ {
		fmt.Printf("--")
	}
	switch ast := n.(type) {
		case *NullExpr:
			fmt.Printf("'null' expr\n")
		case *BasicLit:
			fmt.Printf("Basic Lit :: Value: %q - Kind: %q\n", ast.Value, LitKindToStr[ast.Kind])
		case *ThisExpr:
			fmt.Printf("'this' expr\n")
		case *Name:
			fmt.Printf("ident: '%s'\n", ast.Value)
		case *UnaryExpr:
			fmt.Printf("Unary Expr Kind: %q, Post: '%t'\n", TokenToStr[ast.Kind], ast.Post)
			PrintNode(ast.X, tabs + 1)
		case *CallExpr:
			fmt.Printf("Call Expr\n")
			PrintNode(ast.Func, tabs + 1)
			if ast.ArgList != nil {
				for i:=0; i < tabs; i++ {
					fmt.Printf("--")
				}
				fmt.Printf("Call Expr Arg List\n")
				for i := range ast.ArgList {
					PrintNode(ast.ArgList[i], tabs + 1)
				}
			}
		case *IndexExpr:
			fmt.Printf("Index Expr\n")
			PrintNode(ast.X, tabs + 1)
			PrintNode(ast.Index, tabs + 1)
		case *NameSpaceExpr:
			fmt.Printf("Namespace Expr\n")
			PrintNode(ast.N, tabs + 1)
			PrintNode(ast.Id, tabs + 1)
		case *FieldExpr:
			fmt.Printf("Field Expr\n")
			PrintNode(ast.X, tabs + 1)
			PrintNode(ast.Sel, tabs + 1)
		case *ViewAsExpr:
			fmt.Printf("view_as Expr\n")
			PrintNode(ast.Type, tabs + 1)
			PrintNode(ast.X, tabs + 1)
		case *BinExpr:
			fmt.Printf("Binary Expr - Kind: %q\n", TokenToStr[ast.Kind])
			PrintNode(ast.L, tabs + 1)
			PrintNode(ast.R, tabs + 1)
		case *TernaryExpr:
			fmt.Printf("Ternary Expr\n")
			PrintNode(ast.A, tabs + 1)
			PrintNode(ast.B, tabs + 1)
			PrintNode(ast.C, tabs + 1)
		case *NamedArg:
			fmt.Printf("Named Arg Expr\n")
			PrintNode(ast.Param, tabs + 1)
			PrintNode(ast.X, tabs + 1)
		case *TypedExpr:
			if ast.Tok.Kind==TKIdent {
				fmt.Printf("Typed Expr - Kind: %q\n", ast.Tok.Lexeme)
			} else {
				fmt.Printf("Typed Expr - Kind: %q\n", TokenToStr[ast.Tok.Kind])
			}
		case *CommaExpr:
			fmt.Printf("Comma Expr\n")
			for i := range ast.Exprs {
				PrintNode(ast.Exprs[i], tabs + 1)
			}
	}
}


func Walk(n Node, visitor func(Node) bool) {
	if !visitor(n) {
		return
	}
	
	switch ast := n.(type) {
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
		case *ViewAsExpr:
			Walk(ast.Type, visitor)
			Walk(ast.X, visitor)
		case *BinExpr:
			Walk(ast.L, visitor)
			Walk(ast.R, visitor)
		case *TernaryExpr:
			Walk(ast.A, visitor)
			Walk(ast.B, visitor)
			Walk(ast.C, visitor)
		case *NamedArg:
			Walk(ast.Param, visitor)
			Walk(ast.X, visitor)
		case *CommaExpr:
			for i := range ast.Exprs {
				Walk(ast.Exprs[i], visitor)
			}
	}
}