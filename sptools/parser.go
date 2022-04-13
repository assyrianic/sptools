package SPTools

import (
	"os"
	"fmt"
	"time"
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

func (parser *Parser) checkTokenLen(offset ...int) bool {
	i := 0
	if len(offset) > 0 {
		i = offset[0]
	}
	return parser.idx+i < len(parser.tokens)
}

func (parser *Parser) getEndToken() Token {
	return parser.tokens[len(parser.tokens)-1]
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

func (parser *Parser) want(tk TokenKind, lexeme string) bool {
	if !parser.got(tk) {
		parser.syntaxErr("expecting '%s'", lexeme)
		// continue on and try to parse the remainder
		parser.idx++
		return false
	}
	return true
}

func (parser *Parser) Start() Node {
	return parser.DoBlock()
}


// statement nodes here.
// Statement syntax write here.
type (
	Stmt interface {
		Node
		aStmt()
	}
	
	BadStmt struct {
		stmt
	}
	
	DeleteStmt struct {
		X Expr
		stmt
	}
	
	// { *Stmts }
	BlockStmt struct {
		Stmts []Stmt
		stmt
	}
	
	// if cond body [else/else if body]
	IfStmt struct {
		Cond Expr
		Then, Else Stmt
		stmt
	}
	
	// while cond body
	WhileStmt struct {
		Cond Expr
		Body Stmt
		Do bool // do-while version.
		stmt
	}
	
	// for [init] ; [cond] ; [post] body 
	ForStmt struct {
		Init Stmt
		Cond, Post Expr
		Body Stmt
		stmt
	}
	
	// expr ';'
	ExprStmt struct {
		X Expr
		stmt
	}
	
	// switch cond body
	SwitchStmt struct {
		Cases []Stmt
		Default Stmt
		Cond Expr
		stmt
	}
	
	// case a[, b, ...z]:
	CaseStmt struct {
		Exprs []Expr
		Body Stmt
		stmt
	}
	
	// break, continue
	FlowStmt struct {
		Kind TokenKind
		stmt
	}
	
	// Type i;
	DeclStmt struct {
		Type, Name Expr
		Init Expr // can be nil.
		IsArray bool
		stmt
	}
	
	// return [i];
	RetStmt struct {
		X Expr
		stmt
	}
	
	// assert a;
	AssertStmt struct {
		X Expr
		stmt
	}
	
	// static_assert(a, b);
	StaticAssertStmt struct {
		A, B Expr
		stmt
	}
)
type stmt struct{ node }
func (*stmt) aStmt() {}


func (parser *Parser) noSemi() Stmt {
	parser.syntaxErr("missing ';' semicolon.")
	bad := new(BadStmt)
	copyPosToNode(&bad.node, parser.tokens[parser.idx])
	return bad
}

// BlockStmt = '{' *Statement '}' .
func (parser *Parser) DoBlock() Stmt {
	block := new(BlockStmt)
	parser.want(TKLCurl, "{")
	copyPosToNode(&block.node, parser.tokens[parser.idx-1])
	for parser.idx < len(parser.tokens) && parser.tokens[parser.idx].Kind != TKRCurl {
		time.Sleep(400 * time.Millisecond)
		fmt.Printf("current tok: %v\n", parser.tokens[parser.idx])
		n := parser.Statement()
		if n==nil {
			fmt.Printf("n == nil\n")
			continue
		}
		block.Stmts = append(block.Stmts, n)
	}
	parser.want(TKRCurl, "}")
	return block
}

/*
 * Statement = IfStmt | WhileStmt | ForStmt | SwitchStmt | BlockStmt |
 *             RetStmt | AssertStmt | StaticAssertStmt | DeclStmt | DeleteStmt | ExprStmt .
 */
func (parser *Parser) Statement() Stmt {
	if !parser.checkTokenLen() {
		bad := new(BadStmt)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	switch t := parser.tokens[parser.idx]; t.Kind {
		case TKLCurl:
			return parser.DoBlock()
		case TKIf:
			return parser.DoIf()
		case TKDo, TKWhile:
			return parser.While(t)
		///case TKFor:
		///	return parser.DoFor()
		///case TKForEach:
			// ForEachStmt = 'foreach' '(' DeclStmt 'in' Expr ')' Statement.
		case TKSwitch:
			return parser.Switch()
		case TKReturn:
			ret := new(RetStmt)
			copyPosToNode(&ret.node, t)
			parser.idx++
			if parser.tokens[parser.idx].Kind != TKSemi {
				ret.X = parser.MainExpr()
				if !parser.got(TKSemi) {
					return parser.noSemi()
				}
				return ret
			} else {
				parser.idx++
				return ret
			}
		case TKStaticAssert:
			// 'static_assert' '(' Expr [ ',' Expr ] ')' ';'
			stasrt := new(StaticAssertStmt)
			copyPosToNode(&stasrt.node, t)
			parser.idx++
			parser.want(TKLParen, "(")
			stasrt.A = parser.MainExpr()
			if parser.checkTokenLen() && parser.tokens[parser.idx].Kind==TKComma {
				parser.idx++
				stasrt.B = parser.MainExpr()
			}
			parser.want(TKRParen, ")")
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			return stasrt
		case TKAssert:
			asrt := new(AssertStmt)
			copyPosToNode(&asrt.node, t)
			parser.idx++
			asrt.X = parser.MainExpr()
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			return asrt
		case TKDelete:
			del := new(DeleteStmt)
			copyPosToNode(&del.node, t)
			parser.idx++
			del.X = parser.MainExpr()
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			parser.idx++
			return del
		case TKCase, TKDefault:
			parser.idx++
			fallthrough
		default:
			bad := new(BadStmt)
			copyPosToNode(&bad.node, parser.tokens[parser.idx])
			parser.idx++
			return bad
	}
}

// DoStmt = 'do' Statement 'while' '(' Expr ')' ';' .
// WhileStmt = 'while' '(' Expr ')' Statement .
func (parser *Parser) While(t Token) Stmt {
	while := new(WhileStmt)
	copyPosToNode(&while.node, t)
	parser.idx++
	if t.Kind==TKDo {
		// do-while
		while.Do = true
		while.Body = parser.Statement()
		parser.want(TKWhile, "while")
		parser.want(TKLParen, "(")
		while.Cond = parser.MainExpr()
		parser.want(TKRParen, ")")
		if !parser.got(TKSemi) {
			return parser.noSemi()
		}
	} else {
		// while
		parser.want(TKLParen, "(")
		while.Cond = parser.MainExpr()
		parser.want(TKRParen, ")")
		while.Body = parser.Statement()
	}
	return while
}

// IfStmt = 'if' Expr Statement [ 'else' Statement ] .
func (parser *Parser) DoIf() Stmt {
	parser.want(TKIf, "if")
	ifstmt := new(IfStmt)
	copyPosToNode(&ifstmt.node, parser.tokens[parser.idx-1])
	parser.want(TKLParen, "(")
	ifstmt.Cond = parser.MainExpr()
	parser.want(TKRParen, ")")
	ifstmt.Then = parser.Statement()
	if parser.checkTokenLen() && parser.tokens[parser.idx].Kind==TKElse {
		parser.idx++
		ifstmt.Else = parser.Statement()
		// personally, I'd prefer to fix the not-needing-{ thing but whatever.
		/*
		if parser.checkTokenLen() {
			switch t := parser.tokens[parser.idx]; t.Kind {
				case TKIf:
					ifstmt.Else = parser.DoIf()
				case TKLCurl:
					ifstmt.Else = parser.DoBlock()
				default:
					parser.syntaxErr("ill-formed else block, missing 'if' or { curl.")
					bad := new(BadStmt)
					copyPosToNode(&bad.node, t)
					ifstmt.Else = bad
			}
		}
		*/
	}
	return ifstmt
}

// ForStmt = 'for' '(' [ Decl | Expr ] ';' [ Expr ] ';' [ Expr ] ')' Statement .
func (parser *Parser) DoFor() Stmt {
	parser.want(TKFor, "for")
	forstmt := new(ForStmt)
	copyPosToNode(&forstmt.node, parser.tokens[parser.idx-1])
	parser.want(TKLParen, "(")
	if parser.checkTokenLen() && parser.tokens[parser.idx].Kind != TKSemi {
		//is_decl := false
		if t := parser.tokens[parser.idx]; t.IsType() || t.Kind==TKIdent {
			// https://github.com/alliedmodders/sourcepawn/blob/master/compiler/parser.cpp#L1646
			// checks if token is 'object' keyword.
			// d := parser.Decl()
			///parser.idx++
			parser.want(TKSemi, ";")
		} else {
			// finish after decls are done.
			/**
			switch t.Kind {
				case 
			}
			*/
		}
	}
	return nil
}

// SwitchStmt = 'switch' '(' Expr ')' '{' *CaseClause [ DefaultClause ] '}' .
// CaseClause = 'case' Expr *[ ( ',' | '..' ) Expr ] ':' Statement .
// DefaultClause = 'default' ':' Statement .
func (parser *Parser) Switch() Stmt {
	parser.want(TKSwitch, "switch")
	swtch := new(SwitchStmt)
	copyPosToNode(&swtch.node, parser.tokens[parser.idx-1])
	parser.want(TKLParen, "(")
	swtch.Cond = parser.MainExpr()
	parser.want(TKRParen, ")")
	parser.want(TKLCurl, "{")
	bad_case := false
	for parser.checkTokenLen() && parser.tokens[parser.idx].Kind != TKRCurl {
		switch t := parser.tokens[parser.idx]; t.Kind {
			case TKCase:
				// next do case expressions:
				_case := new(CaseStmt)
				copyPosToNode(&_case.node, parser.tokens[parser.idx])
				parser.idx++
				for parser.checkTokenLen() && parser.tokens[parser.idx].Kind != TKColon {
					if len(_case.Exprs) > 0 {
						parser.want(TKComma, ",")
					}
					
					case_expr := parser.MainExpr()
					if _, is_bad := case_expr.(*BadExpr); is_bad {
						bad_case = true
						goto errd_case
					}
					_case.Exprs = append(_case.Exprs, case_expr)
				}
				if len(_case.Exprs) <= 0 {
					parser.syntaxErr("case is missing expressions!")
					parser.idx++
					bad_case = true
					goto errd_case
				}
				parser.want(TKColon, ":")
				_case.Body = parser.Statement()
				swtch.Cases = append(swtch.Cases, _case)
			case TKDefault:
				parser.idx++
				parser.want(TKColon, ":")
				swtch.Default = parser.Statement()
			default:
				parser.syntaxErr("bad switch label.")
				bad := new(BadStmt)
				copyPosToNode(&bad.node, parser.tokens[parser.idx])
				return bad
		}
	}
errd_case:
	if bad_case {
		bad := new(BadStmt)
		copyPosToNode(&bad.node, parser.tokens[parser.idx])
		return bad
	}
	return swtch
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
	
	// f(a,b,...z)
	CallExpr struct {
		ArgList []Expr // nil means no arguments
		Func      Expr
		expr
	}
	
	// this.a
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


// Expr = AssignExpr *( ',' AssignExpr ) .
func (parser *Parser) MainExpr() Expr {
	a := parser.AssignExpr()
	if parser.checkTokenLen() && parser.tokens[parser.idx].Kind==TKComma {
		c := new(CommaExpr)
		copyPosToNode(&c.node, parser.tokens[parser.idx])
		c.Exprs = append(c.Exprs, a)
		for parser.checkTokenLen() && parser.tokens[parser.idx].Kind==TKComma {
			parser.idx++
			c.Exprs = append(c.Exprs, parser.AssignExpr())
		}
		a = c
	}
	return a
}

// AssignExpr = SubMainExpr *( '['+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '<<' | '>>' | '>>>' ] =' SubMainExpr ) .
func (parser *Parser) AssignExpr() Expr {
	a := parser.SubMainExpr()
	for parser.checkTokenLen() && (parser.tokens[parser.idx].Kind >= TKAssign && parser.tokens[parser.idx].Kind <= TKShLRA) {
		t := parser.tokens[parser.idx]
		parser.idx++
		assign_expr := new(BinExpr)
		copyPosToNode(&assign_expr.node, t)
		assign_expr.L = a
		assign_expr.Kind = t.Kind
		assign_expr.R = parser.SubMainExpr()
		a = assign_expr
	}
	return a
}

// SubMainExpr = LogicalOrExpr [ '?' LogicalOrExpr ':' Expr ] .
func (parser *Parser) SubMainExpr() Expr {
	a := parser.LogicalOrExpr()
	if parser.checkTokenLen() && parser.tokens[parser.idx].Kind==TKQMark {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.LogicalAndExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && t.Kind==TKOrL; t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.EqualExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && t.Kind==TKAndL; t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.RelExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && (t.Kind==TKEq || t.Kind==TKNotEq); t = parser.tokens[parser.idx] {
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

// RelExpr = BitOrExpr *( ( '<[=]' | '>[=]' ) BitOrExpr ) .
func (parser *Parser) RelExpr() Expr {
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.BitOrExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && (t.Kind>=TKLess && t.Kind<=TKLessE); t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.BitXorExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && t.Kind==TKOr; t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.BitAndExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && t.Kind==TKXor; t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.ShiftExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && t.Kind==TKAnd; t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.AddExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && (t.Kind>=TKShAL && t.Kind<=TKShLR); t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.MulExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && (t.Kind==TKAdd || t.Kind==TKSub); t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
	}
	
	e := parser.PrefixExpr()
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && (t.Kind==TKMul || t.Kind==TKDiv || t.Kind==TKMod); t = parser.tokens[parser.idx] {
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
	if !parser.checkTokenLen() {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
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
		bad := new(BadExpr)
		copyPosToNode(&bad.node, t)
		ret_expr = bad
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
	
	if !parser.checkTokenLen() {
		return n
	}
	
	for t := parser.tokens[parser.idx]; parser.checkTokenLen() && (t.Kind==TKDot || t.Kind==TKLBrack || t.Kind==TKLParen || t.Kind==TK2Colons || t.Kind==TKIncr || t.Kind==TKDecr); t = parser.tokens[parser.idx] {
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
				for parser.checkTokenLen() && parser.tokens[parser.idx].Kind != TKRParen {
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
	if !parser.checkTokenLen() || parser.tokens[parser.idx].Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, parser.getEndToken())
		return bad
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
			bad := new(BadExpr)
			copyPosToNode(&bad.node, prim)
			ret_expr = bad
	}
	parser.idx++
	return ret_expr
}

func printTabs(c rune, tabs int) {
	for i:=0; i < tabs; i++ {
		fmt.Printf("%c%c", c, c)
	}
}

func PrintNode(n Node, tabs int) {
	const c = '-'
	printTabs(c, tabs)
	switch ast := n.(type) {
		case *NullExpr:
			fmt.Printf("'null' expr\n")
		case *BasicLit:
			fmt.Printf("Basic Lit :: Value: %q - Kind: %q\n", ast.Value, LitKindToStr[ast.Kind])
		case *ThisExpr:
			fmt.Printf("'this' expr\n")
		case *Name:
			fmt.Printf("Ident: '%s'\n", ast.Value)
		case *UnaryExpr:
			fmt.Printf("Unary Expr Kind: %q, Post: '%t'\n", TokenToStr[ast.Kind], ast.Post)
			PrintNode(ast.X, tabs + 1)
		case *CallExpr:
			fmt.Printf("Call Expr\n")
			PrintNode(ast.Func, tabs + 1)
			if ast.ArgList != nil {
				printTabs(c, tabs)
				fmt.Printf("Call Expr Arg List\n")
				for i := range ast.ArgList {
					PrintNode(ast.ArgList[i], tabs + 1)
				}
			}
		case *IndexExpr:
			fmt.Printf("Index Expr Obj\n")
			PrintNode(ast.X, tabs + 1)
			printTabs(c, tabs)
			fmt.Printf("Index Expr Index\n")
			PrintNode(ast.Index, tabs + 1)
		case *NameSpaceExpr:
			fmt.Printf("Namespace Expr Name\n")
			PrintNode(ast.N, tabs + 1)
			printTabs(c, tabs)
			fmt.Printf("Namespace Expr Id\n")
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
		case *RetStmt:
			fmt.Printf("Return Statement\n")
			if ast.X != nil {
				PrintNode(ast.X, tabs + 1)
			}
		case *IfStmt:
			fmt.Printf("If Statement\n")
			PrintNode(ast.Cond, tabs + 1)
			PrintNode(ast.Then, tabs + 1)
			if ast.Else != nil {
				printTabs(c, tabs)
				fmt.Printf("If Statement :: Else\n")
				PrintNode(ast.Else, tabs + 1)
			}
		case *WhileStmt:
			fmt.Printf("While Statement: is Do-While? %t\n", ast.Do)
			PrintNode(ast.Cond, tabs + 1)
			PrintNode(ast.Body, tabs + 1)
		case *ForStmt:
			fmt.Printf("For Statement\n")
			if ast.Init != nil {
				PrintNode(ast.Init, tabs + 1)
			}
			if ast.Cond != nil {
				PrintNode(ast.Cond, tabs + 1)
			}
			if ast.Post != nil {
				PrintNode(ast.Post, tabs + 1)
			}
			PrintNode(ast.Body, tabs + 1)
		case *ExprStmt:
			fmt.Printf("Expr Statement\n")
			PrintNode(ast.X, tabs + 1)
		case *BlockStmt:
			fmt.Printf("Block Statement\n")
			for i := range ast.Stmts {
				PrintNode(ast.Stmts[i], tabs + 1)
			}
		case *DeleteStmt:
			fmt.Printf("Delete Statement\n")
			PrintNode(ast.X, tabs + 1)
		case *SwitchStmt:
			fmt.Printf("Switch Statement Condition\n")
			PrintNode(ast.Cond, tabs + 1)
			printTabs(c, tabs)
			fmt.Printf("Switch Statement Cases\n")
			for i := range ast.Cases {
				PrintNode(ast.Cases[i], tabs + 1)
			}
			if ast.Default != nil {
				printTabs(c, tabs)
				fmt.Printf("Switch Statement Default Case\n")
				PrintNode(ast.Default, tabs + 1)
			}
		case *CaseStmt:
			fmt.Printf("Case Statement Exprs\n")
			for i := range ast.Exprs {
				PrintNode(ast.Exprs[i], tabs + 1)
			}
			printTabs(c, tabs)
			fmt.Printf("Case Statement Body\n")
			PrintNode(ast.Body, tabs + 1)
		case *FlowStmt:
			fmt.Printf("Flow Statement: %q\n", TokenToStr[ast.Kind])
		case *AssertStmt:
			fmt.Printf("Assert Statement\n")
			PrintNode(ast.X, tabs + 1)
		case *StaticAssertStmt:
			fmt.Printf("Static Assert Statement\n")
			PrintNode(ast.A, tabs + 1)
			PrintNode(ast.B, tabs + 1)
		default:
			fmt.Printf("default :: %T\n", ast)
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
		case *RetStmt:
			if ast.X != nil {
				Walk(ast.X, visitor)
			}
		case *IfStmt:
			Walk(ast.Cond, visitor)
			Walk(ast.Then, visitor)
			if ast.Else != nil {
				Walk(ast.Else, visitor)
			}
		case *WhileStmt:
			Walk(ast.Cond, visitor)
			Walk(ast.Body, visitor)
		case *ForStmt:
			if ast.Init != nil {
				Walk(ast.Init, visitor)
			}
			if ast.Cond != nil {
				Walk(ast.Cond, visitor)
			}
			if ast.Post != nil {
				Walk(ast.Post, visitor)
			}
			Walk(ast.Body, visitor)
		case *ExprStmt:
			Walk(ast.X, visitor)
		case *BlockStmt:
			for i := range ast.Stmts {
				Walk(ast.Stmts[i], visitor)
			}
		case *DeleteStmt:
			Walk(ast.X, visitor)
		case *SwitchStmt:
			Walk(ast.Cond, visitor)
			for i := range ast.Cases {
				Walk(ast.Cases[i], visitor)
			}
			if ast.Default != nil {
				Walk(ast.Default, visitor)
			}
		case *CaseStmt:
			for i := range ast.Exprs {
				Walk(ast.Exprs[i], visitor)
			}
			Walk(ast.Body, visitor)
		case *AssertStmt:
			Walk(ast.X, visitor)
		case *StaticAssertStmt:
			Walk(ast.A, visitor)
			Walk(ast.B, visitor)
	}
}