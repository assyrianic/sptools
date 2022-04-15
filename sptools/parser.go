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

func (parser *Parser) GetToken(offset int) Token {
	tlen := len(parser.tokens)
	if parser.idx + offset >= tlen {
		return parser.tokens[tlen - 1]
	} else {
		return parser.tokens[parser.idx + offset]
	}
}

func (parser *Parser) Advance() Token {
	parser.idx++
	return parser.GetToken(0)
}

func (parser *Parser) got(tk TokenKind) bool {
	if token := parser.GetToken(0); token.Kind==tk {
		parser.idx++
		return true
	}
	return false
}

func (parser *Parser) syntaxErr(msg string, args ...interface{}) {
	token := parser.GetToken(-1)
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


// top-level plugin.
type File struct {
	Decls []Decl
	node
}


type StorageClassFlags int
const (
	IsPublic = StorageClassFlags(1 << iota)
	IsConst
	IsNative
	IsForward
	IsStatic
	IsStock
	IsPrivate
	IsProtected
	IsReadOnly
	IsSealed
	IsVirtual
)
func storageClassFromToken(tok Token) StorageClassFlags {
	switch tok.Kind {
		case TKConst:
			return IsConst
		case TKStock:
			return IsStock
		case TKPublic:
			return IsPublic
		case TKPrivate:
			return IsPrivate
		case TKProtected:
			return IsProtected
		case TKStatic:
			return IsStatic
		case TKForward:
			return IsForward
		case TKNative:
			return IsNative
		case TKReadOnly:
			return IsReadOnly
		case TKSealed:
			return IsSealed
		case TKVirtual:
			return IsVirtual
		default:
			return StorageClassFlags(0)
	}
}


// Declarations here.
type (
	Decl interface {
		Node
		aDecl()
	}
	
	BadDecl struct {
		decl
	}
	
	TypeDecl struct {
		Type Spec // 
		decl
	}
	
	// name1, name2[n], name3=expr;
	VarDecl struct {
		VarType Spec // *VarSpec
		Names, Inits []Expr
		// nil index if there was no initializer/arraydims.
		decl
	}
	
	// class type name() {}
	// class type name();
	// class type name1() = name2;
	FuncDecl struct {
		RetType Spec // *TypeSpec.
		Params []VarDecl
		Body Node // Expr if alias, Stmt if body, nil if forward/native.
		ClassFlags StorageClassFlags
		IsOpOverload bool
		decl
	}
)
type decl struct{ node }
func (*decl) aDecl() {}


// Specifications here.
// Spec represents a constant, type, or variable declaration.
type (
	Spec interface {
		Node
		aSpec()
	}
	
	BadSpec struct {
		spec
	}
	
	// represents a partial variable declaration.
	VarSpec struct {
		Type Spec // *TypeSpec
		ClassFlags StorageClassFlags
		spec
	}
	
	// enum Name { ... }
	// enum { ... }
	EnumSpec struct {
		Ident Expr // can be nil.
		Values []Expr
		spec
	}
	
	// struct Name { ... }
	// enum struct Name { ... }
	StructSpec struct {
		Ident Expr
		IsEnum bool
		Fields []Decl // []*VarDecl
		Methods []Decl // []*FuncDecl
		spec
	}
	
	// methodmap Name [< type] { ... };
	MethodMapSpec struct {
		Ident, Inheritor Expr
		spec
	}
	
	// property Type name {}
	MethodMapPropSpec struct {
		Type Spec // *TypeSpec
		Ident Expr
		spec
	}
	
	// using name;
	UsingSpec struct {
		Namespace Expr
		spec
	}
	
	// type[]&
	TypeSpec struct {
		Type Expr
		Dims int
		IsRef bool
		spec
	}
	
	// typedef name = type;
	TypedefSpec struct {
		spec
	}
	
	// typeset name {}
	TypeSetSpec struct {
		spec
	}
)
type spec struct{ node }
func (*spec) aSpec() {}


// StorageClass = 'native' | 'forward' | 'const' | 'static' | 'stock' | 'public' | 'private' | 'protected' | 'readonly' | 'sealed' | 'virtual' .
func (parser *Parser) StorageClass() StorageClassFlags {
	flags := StorageClassFlags(0)
	for parser.GetToken(0).IsStorageClass() {
		flags |= storageClassFromToken(parser.GetToken(0))
		parser.idx++
	}
	return flags
}

// AbstractDecl = Type [ *'[]' | '&' ] .
func (parser *Parser) AbstractDecl() Spec {
	tspec := new(TypeSpec)
	copyPosToNode(&tspec.node, parser.GetToken(0))
	// next get type name.
	tspec.Type = parser.TypeExpr(false)
	
	// check pre-identifier array dims or ampersand reference.
	if t := parser.GetToken(0); t.Kind==TKLBrack {
		for parser.GetToken(0).Kind==TKLBrack {
			tspec.Dims++
			parser.want(TKLBrack, "[")
			parser.want(TKRBrack, "]")
			tspec.IsRef = true
		}
	} else if t.Kind==TKAnd {
		tspec.IsRef = true
		parser.idx++
	}
	return tspec
}


// VarDecl = VarSpec VarDeclarator .
func (parser *Parser) DoVarDecl() Decl {
	vdecl := new(VarDecl)
	copyPosToNode(&vdecl.node, parser.GetToken(0))
	vdecl.VarType = parser.DoVarSpec()
	parser.DoVarDeclarator(vdecl)
	return vdecl
}

// VarSpec = *StorageClass AbstractDecl .
func (parser *Parser) DoVarSpec() Spec {
	vspec := new(VarSpec)
	copyPosToNode(&vspec.node, parser.GetToken(0))
	vspec.ClassFlags = parser.StorageClass()
	vspec.Type = parser.AbstractDecl()
	return vspec
}

// VarDeclarator = Ident [ IndexExpr ] [ Initializer ] *( ',' VarDeclarator ) .
// Initializer = '=' SubMainExpr | '{' Expr [ ',' ( '...' | *Expr ) ] '}' .
func (parser *Parser) DoVarDeclarator(vdecl *VarDecl) {
	vdecl.Names = append(vdecl.Names, parser.SubMainExpr())
	if parser.GetToken(0).Kind==TKAssign {
		parser.idx++
		vdecl.Inits = append(vdecl.Inits, parser.SubMainExpr())
	} else {
		vdecl.Inits = append(vdecl.Inits, nil)
	}
	
	// TODO: clean this up.
	if parser.GetToken(0).Kind==TKComma {
		for parser.GetToken(0).Kind==TKComma {
			parser.idx++
			vdecl.Names = append(vdecl.Names, parser.SubMainExpr())
			if parser.GetToken(0).Kind==TKAssign {
				parser.idx++
				// { expr list } .
				if parser.GetToken(0).Kind==TKLCurl {
					parser.idx++
					for parser.GetToken(0).Kind != TKRCurl {
						vdecl.Inits = append(vdecl.Inits, parser.MainExpr())
					}
					parser.want(TKRCurl, "}")
				} else {
					// = expr .
					vdecl.Inits = append(vdecl.Inits, parser.SubMainExpr())
				}
			} else {
				vdecl.Inits = append(vdecl.Inits, nil)
			}
		}
	}
}


// FuncDecl = *StorageClass AbstractDecl Ident '(' *VarDecl ')' [ Initializer | Block | ';' ] .
func (parser *Parser) FuncDeclaration() Decl {
	fdecl := new(FuncDecl)
	copyPosToNode(&fdecl.node, parser.GetToken(0))
	return fdecl
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
		Init Node
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
		Case Expr // single or comma expression.
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
		D Decl // VarSpec, TypeDecl, or FuncDecl
		stmt
	}
	
	// return i;
	// return;
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
	copyPosToNode(&bad.node, parser.GetToken(-1))
	return bad
}

// BlockStmt = '{' *Statement '}' .
func (parser *Parser) DoBlock() Stmt {
	block := new(BlockStmt)
	parser.want(TKLCurl, "{")
	copyPosToNode(&block.node, parser.tokens[parser.idx-1])
	for parser.GetToken(0).Kind != TKRCurl {
		time.Sleep(400 * time.Millisecond)
		fmt.Printf("current tok: %v\n", parser.GetToken(0))
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadStmt)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	switch t := parser.GetToken(0); t.Kind {
		case TKConst, TKPublic, TKPrivate, TKProtected, TKForward, TKNative, TKReadOnly, TKSealed, TKVirtual:
			fallthrough
		case TKInt, TKInt8, TKInt16, TKInt32, TKInt64, TKIntN:
			fallthrough
		case TKUInt8, TKUInt16, TKUInt32, TKUInt64, TKChar, TKDouble, TKVoid, TKObject, TKDecl, TKStatic, TKVar:
			// parse declaration.
			vardecl := new(DeclStmt)
			copyPosToNode(&vardecl.node, t)
			vardecl.D = parser.DoVarDecl()
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			return vardecl
		case TKLCurl:
			return parser.DoBlock()
		case TKIf:
			return parser.DoIf()
		case TKDo, TKWhile:
			return parser.While(t)
		case TKFor:
			return parser.DoFor()
		///case TKForEach:
			// ForEachStmt = 'foreach' '(' DeclStmt 'in' Expr ')' Statement.
		case TKSwitch:
			return parser.Switch()
		case TKReturn:
			// RetStmt = 'return' [ Expr ] ';' .
			ret := new(RetStmt)
			copyPosToNode(&ret.node, t)
			parser.idx++
			if parser.GetToken(0).Kind != TKSemi {
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
			// StaticAssertStmt = 'static_assert' '(' Expr [ ',' Expr ] ')' ';' .
			stasrt := new(StaticAssertStmt)
			copyPosToNode(&stasrt.node, t)
			parser.idx++
			parser.want(TKLParen, "(")
			stasrt.A = parser.MainExpr()
			if parser.GetToken(0).Kind==TKComma {
				parser.idx++
				stasrt.B = parser.MainExpr()
			}
			parser.want(TKRParen, ")")
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			return stasrt
		case TKAssert:
			// AssertStmt = 'assert' Expr ';' .
			asrt := new(AssertStmt)
			copyPosToNode(&asrt.node, t)
			parser.idx++
			asrt.X = parser.MainExpr()
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			return asrt
		case TKDelete:
			// DeleteStmt = 'delete' Expr ';' .
			del := new(DeleteStmt)
			copyPosToNode(&del.node, t)
			parser.idx++
			del.X = parser.MainExpr()
			if !parser.got(TKSemi) {
				return parser.noSemi()
			}
			parser.idx++
			return del
		case TKIdent:
			// vast majority of the times,
			// an expression starts with an identifier.
			if t2 := parser.GetToken(1); t2.Kind==TKIdent {
				// possible var decl with custom type.
				vardecl := new(DeclStmt)
				copyPosToNode(&vardecl.node, t)
				vardecl.D = parser.DoVarDecl()
				if !parser.got(TKSemi) {
					return parser.noSemi()
				}
				return vardecl
			} else {
				exp := new(ExprStmt)
				copyPosToNode(&exp.node, t)
				exp.X = parser.MainExpr()
				if !parser.got(TKSemi) {
					return parser.noSemi()
				}
				return exp
			}
		case TKCase, TKDefault, TKSemi:
			// lone semicolon is considered an error.
			parser.idx++
			fallthrough
		default:
			bad := new(BadStmt)
			copyPosToNode(&bad.node, parser.GetToken(0))
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
	if parser.GetToken(0).Kind==TKElse {
		parser.idx++
		ifstmt.Else = parser.Statement()
		// personally, I'd prefer to fix the not-needing-{ thing but whatever.
		/*
		switch t := parser.GetToken(0); t.Kind {
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
	if parser.GetToken(0).Kind != TKSemi {
		if t := parser.GetToken(0); t.IsType() || t.IsStorageClass() || (t.Kind==TKIdent && parser.GetToken(1).Kind==TKIdent) {
			forstmt.Init = parser.DoVarDecl()
		} else {
			forstmt.Init = parser.MainExpr()
		}
	}
	parser.want(TKSemi, ";")
	if parser.GetToken(0).Kind != TKSemi {
		forstmt.Cond = parser.MainExpr()
	}
	parser.want(TKSemi, ";")
	if parser.GetToken(0).Kind != TKRParen {
		forstmt.Post = parser.MainExpr()
	}
	parser.want(TKRParen, ")")
	forstmt.Body = parser.Statement()
	return forstmt
}

// SwitchStmt = 'switch' '(' Expr ')' '{' *CaseClause '}' .
// CaseClause = 'case' ExprList ':' Statement | 'default' ':' Statement .
func (parser *Parser) Switch() Stmt {
	parser.want(TKSwitch, "switch")
	swtch := new(SwitchStmt)
	copyPosToNode(&swtch.node, parser.tokens[parser.idx-1])
	parser.want(TKLParen, "(")
	swtch.Cond = parser.MainExpr()
	parser.want(TKRParen, ")")
	parser.want(TKLCurl, "{")
	bad_case := false
	for parser.GetToken(0).Kind != TKRCurl {
		switch t := parser.GetToken(0); t.Kind {
			case TKCase:
				// next do case expressions:
				_case := new(CaseStmt)
				copyPosToNode(&_case.node, parser.GetToken(0))
				parser.idx++
				case_expr := parser.MainExpr()
				if _, is_bad := case_expr.(*BadExpr); is_bad {
					bad_case = true
					goto errd_case
				} else {
					_case.Case = case_expr
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
				copyPosToNode(&bad.node, parser.GetToken(0))
				return bad
		}
	}
errd_case:
	if bad_case {
		bad := new(BadStmt)
		copyPosToNode(&bad.node, parser.GetToken(0))
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
	
	// ...
	EllipsesExpr struct {
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
	if parser.GetToken(0).Kind==TKComma {
		c := new(CommaExpr)
		copyPosToNode(&c.node, parser.GetToken(0))
		c.Exprs = append(c.Exprs, a)
		for parser.GetToken(0).Kind==TKComma {
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
	for t := parser.GetToken(0); t.Kind >= TKAssign && t.Kind <= TKShLRA; t = parser.GetToken(0) {
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

// SubMainExpr = LogicalOrExpr [ TernaryExpr ] .
func (parser *Parser) SubMainExpr() Expr {
	a := parser.LogicalOrExpr()
	if parser.GetToken(0).Kind==TKQMark {
		// ternary
		a = parser.DoTernary(a)
	}
	return a
}

// TernaryExpr = '?' LogicalOrExpr ':' Expr .
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.LogicalAndExpr()
	for t := parser.GetToken(0); t.Kind==TKOrL; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.EqualExpr()
	for t := parser.GetToken(0); t.Kind==TKAndL; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.RelExpr()
	for t := parser.GetToken(0); t.Kind==TKEq || t.Kind==TKNotEq; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.BitOrExpr()
	for t := parser.GetToken(0); t.Kind>=TKLess && t.Kind<=TKLessE; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.BitXorExpr()
	for t := parser.GetToken(0); t.Kind==TKOr; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.BitAndExpr()
	for t := parser.GetToken(0); t.Kind==TKXor; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.ShiftExpr()
	for t := parser.GetToken(0); t.Kind==TKAnd; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.AddExpr()
	for t := parser.GetToken(0); t.Kind>=TKShAL && t.Kind<=TKShLR; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.MulExpr()
	for t := parser.GetToken(0); t.Kind==TKAdd || t.Kind==TKSub; t = parser.GetToken(0) {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	e := parser.PrefixExpr()
	for t := parser.GetToken(0); t.Kind==TKMul || t.Kind==TKDiv || t.Kind==TKMod; t = parser.GetToken(0) {
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
	switch t := parser.GetToken(0); t.Kind {
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
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	ret_expr := Expr(nil)
	if need_carots {
		parser.want(TKLess, "<")
	}
	if t := parser.GetToken(0); t.IsType() || t.Kind==TKIdent {
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
	if t := parser.GetToken(0); t.Kind==TKViewAs {
		n = parser.ViewAsExpr()
	} else {
		n = parser.PrimaryExpr()
	}
	
	for t := parser.GetToken(0); t.Kind==TKDot || t.Kind==TKLBrack || t.Kind==TKLParen || t.Kind==TK2Colons || t.Kind==TKIncr || t.Kind==TKDecr; t = parser.GetToken(0) {
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
				if parser.GetToken(0).Kind != TKRBrack {
					arr.Index = parser.MainExpr()
				}
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
				for parser.GetToken(0).Kind != TKRParen {
					if len(call.ArgList) > 0 {
						parser.want(TKComma, ",")
					}
					// SP allows setting your params by name.
					// '.param_name = expression'
					// '.' Name '=' Expr
					if parser.GetToken(0).Kind==TKDot {
						parser.idx++
						named_arg := new(NamedArg)
						copyPosToNode(&named_arg.node, parser.tokens[parser.idx-1])
						if iden := parser.GetToken(0); iden.Kind != TKIdent {
							parser.syntaxErr("expected identifier for named arg.")
						}
						named_arg.X = parser.AssignExpr()
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

// Primary = int_lit | rune_lit | string_lit | identifier | 'true' | 'false' | 'this' | 'null' | '...' | '(' Expr ')' .
func (parser *Parser) PrimaryExpr() Expr {
	ret_expr := Expr(nil)
	if tIsEoF := parser.GetToken(0); tIsEoF.Kind==TKEoF {
		bad := new(BadExpr)
		copyPosToNode(&bad.node, tIsEoF)
		return bad
	}
	
	if parser.GetToken(0).IsType() {
		return parser.TypeExpr(false)
	}
	switch prim := parser.GetToken(0); prim.Kind {
		case TKEllipses:
			ell := new(EllipsesExpr)
			copyPosToNode(&ell.node, prim)
			ret_expr = ell
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
		case nil:
			fmt.Printf("nil Node\n")
		case *BadStmt:
			fmt.Printf("Bad Stmt Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *BadExpr:
			fmt.Printf("Bad Expr Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *BadSpec:
			fmt.Printf("Bad Spec Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *BadDecl:
			fmt.Printf("Bad Decl Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
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
		case *EllipsesExpr:
			fmt.Printf("Ellipses '...' Expr\n")
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
				printTabs(c, tabs)
				fmt.Printf("For Statement Init\n")
				PrintNode(ast.Init, tabs + 1)
			}
			if ast.Cond != nil {
				printTabs(c, tabs)
				fmt.Printf("For Statement Cond\n")
				PrintNode(ast.Cond, tabs + 1)
			}
			if ast.Post != nil {
				printTabs(c, tabs)
				fmt.Printf("For Statement Post\n")
				PrintNode(ast.Post, tabs + 1)
			}
			printTabs(c, tabs)
			fmt.Printf("For Statement Body\n")
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
			PrintNode(ast.Case, tabs + 1)
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
		case *DeclStmt:
			fmt.Printf("Declaration Statement\n")
			PrintNode(ast.D, tabs + 1)
		case *VarSpec:
			fmt.Printf("Var Specification Type\n")
			PrintNode(ast.Type, tabs + 1)
			printTabs(c, tabs)
			fmt.Printf("Var Specification Flags:: %d\n", ast.ClassFlags)
		case *TypeSpec:
			fmt.Printf("Type Specification\n")
			PrintNode(ast.Type, tabs + 1)
			printTabs(c, tabs)
			fmt.Printf("Type Specification Dims:: %d\n", ast.Dims)
			printTabs(c, tabs)
			fmt.Printf("Type Specification Is Reference:: %t\n", ast.IsRef)
		case *VarDecl:
			fmt.Printf("Var Declaration Type\n")
			PrintNode(ast.VarType, tabs + 1)
			printTabs(c, tabs)
			fmt.Printf("Var Specification Names\n")
			for i := range ast.Names {
				PrintNode(ast.Names[i], tabs + 1)
			}
			printTabs(c, tabs)
			fmt.Printf("Var Specification Inits\n")
			for i := range ast.Inits {
				PrintNode(ast.Inits[i], tabs + 1)
			}
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
			Walk(ast.Case, visitor)
			Walk(ast.Body, visitor)
		case *AssertStmt:
			Walk(ast.X, visitor)
		case *StaticAssertStmt:
			Walk(ast.A, visitor)
			Walk(ast.B, visitor)
		case *DeclStmt:
			Walk(ast.D, visitor)
		case *VarSpec:
			Walk(ast.Type, visitor)
		case *TypeSpec:
			Walk(ast.Type, visitor)
		case *VarDecl:
			Walk(ast.VarType, visitor)
			for i := range ast.Names {
				if ast.Names[i] != nil {
					Walk(ast.Names[i], visitor)
				}
			}
			for i := range ast.Inits {
				if ast.Inits[i] != nil {
					Walk(ast.Inits[i], visitor)
				}
			}
	}
}