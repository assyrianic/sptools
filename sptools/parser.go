package SPTools

import (
	"os"
	"io"
	"fmt"
	"time"
	"strings"
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
	index := parser.idx + offset
	if index >= tlen || index < 0 {
		return parser.tokens[tlen - 1]
	}
	
	if t := parser.tokens[index]; t.Kind==TKNewline || t.Kind==TKComment {
		parser.idx++
		return parser.GetToken(offset)
	}
	return parser.tokens[index]
}

func (parser *Parser) Advance(i int) Token {
	parser.idx += i
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
	if parser.Errs > 20 {
		writeMsg(&parser.Errs, os.Stdout, *token.Path, "syntax error", COLOR_RED, &token.Line, &token.Col, "too many errors!")
		os.Exit(-1)
	}
}

func (parser *Parser) want(tk TokenKind, lexeme string) bool {
	if !parser.got(tk) {
		parser.syntaxErr("expecting '%s' but got '%s'", lexeme, parser.GetToken(0).Lexeme)
		// continue on and try to parse the remainder
		parser.idx++
		return false
	}
	return true
}


func (parser *Parser) Start() Node {
	return parser.TopDecl()
}


// top-level plugin.
type Plugin struct {
	Decls []Decl
	Funcs map[string]*FuncDecl
	Vars  map[string]*VarDecl
	Types map[string]*TypeDecl
	node
}

// Plugin = +TopDecl .
// TopDecl = FuncDecl | TypeDecl | VarDecl .
func (parser *Parser) TopDecl() Node {
	plugin := new(Plugin)
	for t := parser.GetToken(0); t.Kind != TKEoF; t = parser.GetToken(0) {
		time.Sleep(100 * time.Millisecond)
		///fmt.Printf("TopDecl :: current tok: %v\n", t)
		if t.IsStorageClass() || t.IsType() || t.Kind==TKIdent && parser.GetToken(1).Kind==TKIdent {
			v_or_f_decl := parser.DoVarOrFuncDecl(false)
			plugin.Decls = append(plugin.Decls, v_or_f_decl)
		} else {
			type_decl := new(TypeDecl)
			copyPosToNode(&type_decl.node, t)
			switch t.Kind {
				///case TKMethodMap:
				case TKTypedef:
					type_decl.Type = parser.DoTypedef()
				case TKTypeset:
					type_decl.Type = parser.DoTypeSet()
				case TKEnum:
					type_decl.Type = parser.DoEnumSpec()
				case TKStruct:
					type_decl.Type = parser.DoStruct(false)
				case TKUsing:
					type_decl.Type = parser.DoUsingSpec()
				default:
					bad := new(BadDecl)
					copyPosToNode(&bad.node, t)
					///fmt.Printf("TopDecl :: bad Node: %s | Line: %d, Col: %d\n", t.Lexeme, t.Line, t.Col)
					plugin.Decls = append(plugin.Decls, bad)
					return plugin
			}
			plugin.Decls = append(plugin.Decls, type_decl)
		}
	}
	return plugin
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

func (sc StorageClassFlags) String() string {
	var sb strings.Builder
	n := sc
	for n != 0 {
		if sb.Len() > 0 {
			sb.WriteString(" ")
		}
		switch {
			case n & IsPublic > 0:
				sb.WriteString("public")
				n &^= IsPublic
			case n & IsConst > 0:
				sb.WriteString("const")
				n &^= IsConst
			case n & IsNative > 0:
				sb.WriteString("native")
				n &^= IsNative
			case n & IsForward > 0:
				sb.WriteString("forward")
				n &^= IsForward
			case n & IsStatic > 0:
				sb.WriteString("static")
				n &^= IsStatic
			case n & IsStock > 0:
				sb.WriteString("stock")
				n &^= IsStock
			case n & IsPrivate > 0:
				sb.WriteString("private")
				n &^= IsPrivate
			case n & IsProtected > 0:
				sb.WriteString("protected")
				n &^= IsProtected
			case n & IsReadOnly > 0:
				sb.WriteString("readonly")
				n &^= IsReadOnly
			case n & IsSealed > 0:
				sb.WriteString("sealed")
				n &^= IsSealed
			case n & IsVirtual > 0:
				sb.WriteString("virtual")
				n &^= IsVirtual
		}
	}
	return sb.String()
}

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
		Type Spec // anything not Func or Var Decl.
		decl
	}
	
	// name1, name2[n], name3=expr;
	VarDecl struct {
		Type Spec // *TypeSpec
		Names, Inits []Expr
		Dims [][]Expr // a var can have multiple dims, account for em all.
		// nil index if there was no initializer.
		ClassFlags StorageClassFlags
		decl
	}
	
	// class type name() {}
	// class type name();
	// class type name1() = name2;
	FuncDecl struct {
		RetType Spec // *TypeSpec.
		Ident Expr
		Params []Decl // []*VarDecl, *BadDecl if error.
		Body Node // Expr if alias, Stmt if body, nil if ';'.
		ClassFlags StorageClassFlags
		decl
	}
)
type decl struct{ node }
func (*decl) aDecl() {}


// VarDecl  = VarOrFuncSpec VarDeclarator .
// FuncDecl = VarOrFuncSpec FuncDeclarator .
func (parser *Parser) DoVarOrFuncDecl(param bool) Decl {
	saved_token := parser.GetToken(0)
	class_flags, spec_type := parser.VarOrFuncSpec()
	ident := parser.PrimaryExpr() // get NAME only.
	if t := parser.GetToken(0); t.Kind==TKLParen {
		fdecl := new(FuncDecl)
		copyPosToNode(&fdecl.node, saved_token)
		fdecl.RetType = spec_type
		fdecl.ClassFlags = class_flags
		fdecl.Ident = ident
		parser.DoFuncDeclarator(fdecl)
		return fdecl
	} else {
		vdecl := new(VarDecl)
		copyPosToNode(&vdecl.node, saved_token)
		vdecl.Type = spec_type
		vdecl.ClassFlags = class_flags
		vdecl.Names = append(vdecl.Names, ident)
		parser.DoVarDeclarator(vdecl, param)
		return vdecl
	}
}

// VarDeclarator = Ident [ IndexExpr ] [ Initializer ] *( ',' VarDeclarator ) .
// Initializer = '=' SubMainExpr | '{' Expr [ ',' ( '...' | *Expr ) ] '}' .
func (parser *Parser) DoVarDeclarator(vdecl *VarDecl, param bool) {
	for {
		if parser.GetToken(0).Kind==TKLBrack {
			var dims []Expr
			for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind==TKLBrack; t = parser.GetToken(0) {
				parser.want(TKLBrack, "[")
				dims = append(dims, parser.SubMainExpr())
				parser.want(TKRBrack, "]")
			}
			vdecl.Dims = append(vdecl.Dims, dims)
		} else {
			vdecl.Dims = append(vdecl.Dims, nil)
		}
		
		if parser.GetToken(0).Kind==TKAssign {
			parser.idx++
			// { expr list } .
			if parser.GetToken(0).Kind==TKLCurl {
				parser.idx++
				for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind != TKRCurl; t = parser.GetToken(0) {
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
		
		if param || parser.GetToken(0).Kind==TKEoF || parser.GetToken(0).Kind != TKComma {
			break
		}
	}
}

// ParamList = '(' *VarDecl ')' .
func (parser *Parser) DoParamList() []Decl {
	var params []Decl
	parser.want(TKLParen, "(")
	for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind != TKRParen; t = parser.GetToken(0) {
		time.Sleep(100 * time.Millisecond)
		if len(params) > 0 {
			parser.want(TKComma, ",")
		}
		
		var_decl := parser.DoVarOrFuncDecl(true)
		if _, is_var_decl := var_decl.(*VarDecl); !is_var_decl {
			bad_decl := new(BadDecl)
			copyPosToNode(&bad_decl.node, t)
			params = append(params, bad_decl)
		} else {
			params = append(params, var_decl)
		}
	}
	parser.want(TKRParen, ")")
	return params
}

// FuncSpec = Ident ParamList ( Initializer | Block | ';' ) .
func (parser *Parser) DoFuncDeclarator(fdecl *FuncDecl) {
	fdecl.Params = parser.DoParamList()
	switch t := parser.GetToken(0); t.Kind {
		case TKSemi:
			parser.want(TKSemi, ";")
			fdecl.Body = nil
		case TKLCurl:
			fdecl.Body = parser.DoBlock()
		case TKAssign:
			parser.idx++
			fdecl.Body = parser.MainExpr()
		default:
			fdecl.Body = new(BadDecl)
			copyPosToNode(&fdecl.node, t)
	}
}


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
	
	// enum Name { ... }
	// enum { ... }
	// enum ( op= i ) { ... }
	EnumSpec struct {
		Ident Expr // can be nil.
		Step Expr // can be nil.
		StepOp TokenKind
		Names []Expr
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
	
	// methodmap Name [< type] { ... };
	MethodMapSpec struct {
		Ident Expr
		Parent Expr // TypeExpr
		Props []Spec // []*MethodMapPropSpec
		Methods []Spec
		spec
	}
	
	// property Type name {}
	MethodMapPropSpec struct {
		Type Spec // *TypeSpec
		Ident Expr
		spec
	}
	
	// function type params;
	SignatureSpec struct {
		Type Spec
		Params []Decl // array of []*VarDecl, *BadDecl if error.
		spec
	}
	
	// typedef name = function type params;
	TypeDefSpec struct {
		Ident Expr
		Sig Spec // *SignatureSpec
		spec
	}
	
	// typeset name {};
	TypeSetSpec struct {
		Ident Expr
		Signatures []Spec // []*FuncSpec
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
	switch t := parser.GetToken(0); t.Kind {
		case TKLBrack:
			for parser.GetToken(0).Kind==TKLBrack {
				tspec.Dims++
				parser.want(TKLBrack, "[")
				parser.want(TKRBrack, "]")
				tspec.IsRef = true
			}
		case TKAnd:
			tspec.IsRef = true
			parser.Advance(1)
	}
	return tspec
}

// VarOrFuncSpec = *StorageClass AbstractDecl .
func (parser *Parser) VarOrFuncSpec() (StorageClassFlags, Spec) {
	return parser.StorageClass(), parser.AbstractDecl()
}

// SignatureSpec = 'function' AbstractDecl ParamsList .
func (parser *Parser) DoFuncSignature() Spec {
	sig := new(SignatureSpec)
	copyPosToNode(&sig.node, parser.GetToken(0))
	parser.want(TKFunction, "function")
	sig.Type = parser.AbstractDecl()
	sig.Params = parser.DoParamList()
	return sig
}

// EnumSpec = 'enum' [ ident [ ':' ] '(' operator PrimaryExpr ')' ] '{' +EnumEntry '}' [ ';' ] .
// EnumEntry = Ident [ '=' Expr ] .
func (parser *Parser) DoEnumSpec() Spec {
	enum := new(EnumSpec)
	copyPosToNode(&enum.node, parser.GetToken(0))
	parser.want(TKEnum, "enum")
	if t := parser.GetToken(0); t.Kind==TKStruct {
		return parser.DoStruct(true)
	}
	
	if parser.GetToken(0).Kind==TKIdent {
		enum.Ident = parser.PrimaryExpr()
	}
	
	if parser.GetToken(0).Kind==TKColon {
		parser.want(TKColon, ":")
	}
	
	if t := parser.GetToken(0); t.Kind==TKLParen {
		if !t.IsOperator() {
			parser.syntaxErr("expected math operator for enum auto-incrementer.")
			bad := new(BadSpec)
			copyPosToNode(&bad.node, parser.GetToken(0))
			return bad
		} else {
			enum.StepOp = t.Kind
		}
		parser.Advance(1)
		enum.Step = parser.SubMainExpr()
		parser.want(TKRParen, ")")
	}
	
	parser.want(TKLCurl, "{")
	for {
		time.Sleep(100 * time.Millisecond)
		if parser.GetToken(0).Kind==TKRCurl {
			break
		}
		
		///fmt.Printf("DoEnumSpec :: current tok: %v\n", parser.GetToken(0))
		enum.Names = append(enum.Names, parser.PrimaryExpr())
		if parser.GetToken(0).Kind==TKAssign {
			parser.Advance(1)
			enum.Values = append(enum.Values, parser.SubMainExpr())
		} else {
			enum.Values = append(enum.Values, nil)
		}
		
		if parser.GetToken(0).Kind==TKComma {
			parser.Advance(1)
		} else {
			break
		}
	}
	parser.want(TKRCurl, "}")
	if parser.GetToken(0).Kind==TKSemi {
		parser.Advance(1)
	}
	return enum
}

// StructSpec = 'struct' Ident '{' *Field '}' [ ';' ] .
// Field = VarDecl ';' | FuncDecl .
func (parser *Parser) DoStruct(is_enum bool) Spec {
	struc := new(StructSpec)
	copyPosToNode(&struc.node, parser.GetToken(0))
	parser.want(TKStruct, "struct")
	struc.IsEnum = is_enum
	struc.Ident = parser.PrimaryExpr()
	parser.want(TKLCurl, "{")
	for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind != TKRCurl; t = parser.GetToken(0) {
		time.Sleep(100 * time.Millisecond)
		///fmt.Printf("DoStruct :: current tok: %v\n", t)
		v_or_f_decl := parser.DoVarOrFuncDecl(false)
		switch ast := v_or_f_decl.(type) {
			case *VarDecl:
				struc.Fields = append(struc.Fields, ast)
				parser.want(TKSemi, ";")
			case *FuncDecl:
				struc.Methods = append(struc.Methods, ast)
			default:
				if is_enum {
					parser.syntaxErr("bad field/method in enum struct")
				} else {
					parser.syntaxErr("bad field/method in struct")
				}
				bad := new(BadSpec)
				copyPosToNode(&bad.node, parser.GetToken(0))
				return bad
		}
	}
	parser.want(TKRCurl, "}")
	if parser.GetToken(0).Kind==TKSemi {
		parser.Advance(1)
	}
	return struc
}

// UsingSpec = 'using' Expr ';' .
func (parser *Parser) DoUsingSpec() Spec {
	using := new(UsingSpec)
	copyPosToNode(&using.node, parser.GetToken(0))
	parser.want(TKUsing, "using")
	using.Namespace = parser.SubMainExpr()
	if !parser.got(TKSemi) {
		parser.syntaxErr("missing ending ';' semicolon for 'using' specification.")
		bad := new(BadSpec)
		copyPosToNode(&bad.node, parser.GetToken(-1))
		return bad
	}
	return using
}

// TypeSetSpec = 'typeset' Ident '{' *( SignatureSpec ';' ) '}' ';' .
func (parser *Parser) DoTypeSet() Spec {
	typeset := new(TypeSetSpec)
	copyPosToNode(&typeset.node, parser.GetToken(0))
	parser.want(TKTypeset, "typeset")
	typeset.Ident = parser.PrimaryExpr()
	parser.want(TKLCurl, "{")
	for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind != TKRCurl; t = parser.GetToken(0) {
		time.Sleep(100 * time.Millisecond)
		///fmt.Printf("DoTypeSet :: current tok: %v\n", t)
		signature := parser.DoFuncSignature()
		typeset.Signatures = append(typeset.Signatures, signature)
		parser.want(TKSemi, ";")
	}
	
	parser.want(TKRCurl, "}")
	if !parser.got(TKSemi) {
		parser.syntaxErr("missing ending ';' semicolon for 'typeset' specification.")
		bad := new(BadSpec)
		copyPosToNode(&bad.node, parser.GetToken(-1))
		return bad
	}
	return typeset
}

// TypeDefSpec = 'typedef' Ident '=' SignatureSpec ';' .
func (parser *Parser) DoTypedef() Spec {
	typedef := new(TypeDefSpec)
	copyPosToNode(&typedef.node, parser.GetToken(0))
	parser.want(TKTypedef, "typedef")
	typedef.Ident = parser.PrimaryExpr()
	parser.want(TKAssign, "=")
	typedef.Sig = parser.DoFuncSignature()
	if !parser.got(TKSemi) {
		parser.syntaxErr("missing ending ';' semicolon for 'typedef' specification.")
		bad := new(BadSpec)
		copyPosToNode(&bad.node, parser.GetToken(-1))
		return bad
	}
	return typedef
}

// MethodMapSpec = 'methodmap' Ident [ '<' TypeExpr ] '{'  '}' [ ';' ] .
func (parser *Parser) DoMethodMap() Spec {
	methodmap := new(MethodMapSpec)
	copyPosToNode(&methodmap.node, parser.GetToken(0))
	parser.want(TKMethodMap, "methodmap")
	methodmap.Ident = parser.PrimaryExpr()
	if parser.GetToken(0).Kind==TKLess {
		parser.Advance(1)
		methodmap.Parent = parser.TypeExpr(false)
	}
	parser.want(TKLCurl, "{")
	
	parser.want(TKRCurl, "}")
	if !parser.got(TKSemi) {
		parser.syntaxErr("missing ending ';' semicolon for 'methodmap' specification.")
		bad := new(BadSpec)
		copyPosToNode(&bad.node, parser.GetToken(-1))
		return bad
	}
	return methodmap
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
	///fmt.Printf("starting tok: %v\n", parser.GetToken(0))
	parser.want(TKLCurl, "{")
	copyPosToNode(&block.node, parser.tokens[parser.idx-1])
	for t := parser.GetToken(0); t.Kind != TKRCurl && t.Kind != TKEoF; t = parser.GetToken(0) {
		time.Sleep(100 * time.Millisecond)
		///fmt.Printf("current tok: %v\n", t)
		n := parser.Statement()
		if n==nil {
			///fmt.Printf("n == nil\n")
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
		case TKConst, TKPublic, TKPrivate, TKProtected, TKForward, TKNative, TKReadOnly, TKSealed, TKVirtual, TKStock:
			fallthrough
		case TKInt, TKInt8, TKInt16, TKInt32, TKInt64, TKIntN:
			fallthrough
		case TKUInt8, TKUInt16, TKUInt32, TKUInt64, TKChar, TKDouble, TKVoid, TKObject, TKDecl, TKStatic, TKVar:
			// parse declaration.
			vardecl := new(DeclStmt)
			copyPosToNode(&vardecl.node, t)
			vardecl.D = parser.DoVarOrFuncDecl(false)
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
		case TKIdent, TKThis:
			// vast majority of the times,
			// an expression starts with an identifier.
			if t2 := parser.GetToken(1); t2.Kind==TKIdent {
				// possible var decl with custom type.
				vardecl := new(DeclStmt)
				copyPosToNode(&vardecl.node, t)
				vardecl.D = parser.DoVarOrFuncDecl(false)
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
			forstmt.Init = parser.DoVarOrFuncDecl(false)
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
	for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind != TKRCurl; t = parser.GetToken(0) {
		switch t.Kind {
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
	parser.want(TKRCurl, "}")
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
		X Expr
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
		for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind==TKComma; t = parser.GetToken(0) {
			time.Sleep(100 * time.Millisecond)
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
		parser.syntaxErr("missing type expression.")
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
				for t := parser.GetToken(0); t.Kind != TKEoF && t.Kind != TKRParen; t = parser.GetToken(0) {
					if len(call.ArgList) > 0 {
						time.Sleep(100 * time.Millisecond)
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
			if t := parser.GetToken(0); t.Kind != TKRParen {
				parser.syntaxErr("missing ending ')' right paren for nested expression")
				bad := new(BadExpr)
				copyPosToNode(&bad.node, parser.GetToken(0))
				ret_expr = bad
			}
		case TKOperator:
			operator := prim.Lexeme
			operator += parser.GetToken(1).Lexeme
			parser.idx++
			iden := new(Name)
			iden.Value = operator
			copyPosToNode(&iden.node, prim)
			ret_expr = iden
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
			parser.syntaxErr("bad primary expression '%s'", prim.Lexeme)
			bad := new(BadExpr)
			copyPosToNode(&bad.node, prim)
			ret_expr = bad
	}
	parser.idx++
	return ret_expr
}

func printTabs(c rune, tabs int, w io.Writer) {
	for i:=0; i < tabs; i++ {
		fmt.Fprintf(w, "%c%c", c, c)
	}
}

func PrintNode(n Node, tabs int, w io.Writer) {
	const c = '-'
	printTabs(c, tabs, w)
	switch ast := n.(type) {
		case nil:
			fmt.Fprintf(w, "nil Node\n")
		case *BadStmt:
			fmt.Fprintf(w, "Bad Stmt Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *BadExpr:
			fmt.Fprintf(w, "Bad Expr Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *BadSpec:
			fmt.Fprintf(w, "Bad Spec Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *BadDecl:
			fmt.Fprintf(w, "Bad Decl Node:: Line: %v | Col: %v\n", ast.node.pos.Line, ast.node.pos.Col)
		case *NullExpr:
			fmt.Fprintf(w, "'null' expr\n")
		case *BasicLit:
			fmt.Fprintf(w, "Basic Lit :: Value: %q - Kind: %q\n", ast.Value, LitKindToStr[ast.Kind])
		case *ThisExpr:
			fmt.Fprintf(w, "'this' expr\n")
		case *Name:
			fmt.Fprintf(w, "Ident: '%s'\n", ast.Value)
		case *UnaryExpr:
			fmt.Fprintf(w, "Unary Expr Kind: %q, Post: '%t'\n", TokenToStr[ast.Kind], ast.Post)
			PrintNode(ast.X, tabs + 1, w)
		case *CallExpr:
			fmt.Fprintf(w, "Call Expr\n")
			PrintNode(ast.Func, tabs + 1, w)
			if ast.ArgList != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Call Expr Arg List\n")
				for i := range ast.ArgList {
					PrintNode(ast.ArgList[i], tabs + 1, w)
				}
			}
		case *IndexExpr:
			fmt.Fprintf(w, "Index Expr Obj\n")
			PrintNode(ast.X, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Index Expr Index\n")
			PrintNode(ast.Index, tabs + 1, w)
		case *NameSpaceExpr:
			fmt.Fprintf(w, "Namespace Expr Name\n")
			PrintNode(ast.N, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Namespace Expr Id\n")
			PrintNode(ast.Id, tabs + 1, w)
		case *FieldExpr:
			fmt.Fprintf(w, "Field Expr\n")
			PrintNode(ast.X, tabs + 1, w)
			PrintNode(ast.Sel, tabs + 1, w)
		case *ViewAsExpr:
			fmt.Fprintf(w, "view_as Expr\n")
			PrintNode(ast.Type, tabs + 1, w)
			PrintNode(ast.X, tabs + 1, w)
		case *BinExpr:
			fmt.Fprintf(w, "Binary Expr - Kind: %q\n", TokenToStr[ast.Kind])
			PrintNode(ast.L, tabs + 1, w)
			PrintNode(ast.R, tabs + 1, w)
		case *TernaryExpr:
			fmt.Fprintf(w, "Ternary Expr\n")
			PrintNode(ast.A, tabs + 1, w)
			PrintNode(ast.B, tabs + 1, w)
			PrintNode(ast.C, tabs + 1, w)
		case *NamedArg:
			fmt.Fprintf(w, "Named Arg Expr\n")
			PrintNode(ast.X, tabs + 1, w)
		case *TypedExpr:
			if ast.Tok.Kind==TKIdent {
				fmt.Fprintf(w, "Typed Expr - Kind: %q\n", ast.Tok.Lexeme)
			} else {
				fmt.Fprintf(w, "Typed Expr - Kind: %q\n", TokenToStr[ast.Tok.Kind])
			}
		case *CommaExpr:
			fmt.Fprintf(w, "Comma Expr\n")
			for i := range ast.Exprs {
				PrintNode(ast.Exprs[i], tabs + 1, w)
			}
		case *EllipsesExpr:
			fmt.Fprintf(w, "Ellipses '...' Expr\n")
		case *RetStmt:
			fmt.Fprintf(w, "Return Statement\n")
			if ast.X != nil {
				PrintNode(ast.X, tabs + 1, w)
			}
		case *IfStmt:
			fmt.Fprintf(w, "If Statement\n")
			PrintNode(ast.Cond, tabs + 1, w)
			PrintNode(ast.Then, tabs + 1, w)
			if ast.Else != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "If Statement :: Else\n")
				PrintNode(ast.Else, tabs + 1, w)
			}
		case *WhileStmt:
			fmt.Fprintf(w, "While Statement: is Do-While? %t\n", ast.Do)
			PrintNode(ast.Cond, tabs + 1, w)
			PrintNode(ast.Body, tabs + 1, w)
		case *ForStmt:
			fmt.Fprintf(w, "For Statement\n")
			if ast.Init != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "For Statement Init\n")
				PrintNode(ast.Init, tabs + 1, w)
			}
			if ast.Cond != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "For Statement Cond\n")
				PrintNode(ast.Cond, tabs + 1, w)
			}
			if ast.Post != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "For Statement Post\n")
				PrintNode(ast.Post, tabs + 1, w)
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "For Statement Body\n")
			PrintNode(ast.Body, tabs + 1, w)
		case *ExprStmt:
			fmt.Fprintf(w, "Expr Statement\n")
			PrintNode(ast.X, tabs + 1, w)
		case *BlockStmt:
			fmt.Fprintf(w, "Block Statement\n")
			for i := range ast.Stmts {
				PrintNode(ast.Stmts[i], tabs + 1, w)
			}
		case *DeleteStmt:
			fmt.Fprintf(w, "Delete Statement\n")
			PrintNode(ast.X, tabs + 1, w)
		case *SwitchStmt:
			fmt.Fprintf(w, "Switch Statement Condition\n")
			PrintNode(ast.Cond, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Switch Statement Cases\n")
			for i := range ast.Cases {
				PrintNode(ast.Cases[i], tabs + 1, w)
			}
			if ast.Default != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Switch Statement Default Case\n")
				PrintNode(ast.Default, tabs + 1, w)
			}
		case *CaseStmt:
			fmt.Fprintf(w, "Case Statement Exprs\n")
			PrintNode(ast.Case, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Case Statement Body\n")
			PrintNode(ast.Body, tabs + 1, w)
		case *FlowStmt:
			fmt.Fprintf(w, "Flow Statement: %q\n", TokenToStr[ast.Kind])
		case *AssertStmt:
			fmt.Fprintf(w, "Assert Statement\n")
			PrintNode(ast.X, tabs + 1, w)
		case *StaticAssertStmt:
			fmt.Fprintf(w, "Static Assert Statement\n")
			PrintNode(ast.A, tabs + 1, w)
			PrintNode(ast.B, tabs + 1, w)
		case *DeclStmt:
			fmt.Fprintf(w, "Declaration Statement\n")
			PrintNode(ast.D, tabs + 1, w)
		case *TypeSpec:
			fmt.Fprintf(w, "Type Specification\n")
			PrintNode(ast.Type, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Type Specification Dims:: %d\n", ast.Dims)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Type Specification Is Reference:: %t\n", ast.IsRef)
		case *EnumSpec:
			fmt.Fprintf(w, "Enum Specification\n")
			PrintNode(ast.Ident, tabs + 1, w)
			if ast.Step != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Enum Specification Step:: Op: %s\n", TokenToStr[ast.StepOp])
				PrintNode(ast.Step, tabs + 1, w)
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Enum Specification Names\n")
			for i := range ast.Names {
				PrintNode(ast.Names[i], tabs + 1, w)
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Enum Specification Values\n")
			for i := range ast.Values {
				PrintNode(ast.Values[i], tabs + 1, w)
			}
		case *StructSpec:
			if ast.IsEnum {
				fmt.Fprintf(w, "Enum Struct Specification\n")
			} else {
				fmt.Fprintf(w, "Struct Specification\n")
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Struct Ident\n")
			PrintNode(ast.Ident, tabs + 1, w)
			if len(ast.Fields) > 0 {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Struct Fields\n")
				for i := range ast.Fields {
					PrintNode(ast.Fields[i], tabs + 1, w)
				}
			}
			if len(ast.Methods) > 0 {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Struct Methods\n")
				for i := range ast.Methods {
					PrintNode(ast.Methods[i], tabs + 1, w)
				}
			}
		case *UsingSpec:
			fmt.Fprintf(w, "Using Specification\n")
			PrintNode(ast.Namespace, tabs + 1, w)
		case *SignatureSpec:
			fmt.Fprintf(w, "Function Signature Specification\n")
			PrintNode(ast.Type, tabs + 1, w)
			if len(ast.Params) > 0 {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Function Signature Params\n")
				for i := range ast.Params {
					PrintNode(ast.Params[i], tabs + 1, w)
				}
			}
		case *TypeDefSpec:
			fmt.Fprintf(w, "TypeDef Specification Ident\n")
			PrintNode(ast.Ident, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "TypeDef Specification Signature\n")
			PrintNode(ast.Sig, tabs + 1, w)
		case *TypeSetSpec:
			fmt.Fprintf(w, "Typeset Specification Ident\n")
			PrintNode(ast.Ident, tabs + 1, w)
			if len(ast.Signatures) > 0 {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Typeset Signatures\n")
				for i := range ast.Signatures {
					PrintNode(ast.Signatures[i], tabs + 1, w)
				}
			}
		case *VarDecl:
			fmt.Fprintf(w, "Var Declaration Type\n")
			PrintNode(ast.Type, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Var Declaration Names\n")
			for i := range ast.Names {
				PrintNode(ast.Names[i], tabs + 1, w)
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Var Declaration Array Dims\n")
			for i := range ast.Dims {
				if ast.Dims[i] != nil {
					for _, dim := range ast.Dims[i] {
						PrintNode(dim, tabs + 1, w)
					}
				}
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Var Declaration Inits\n")
			for i := range ast.Inits {
				PrintNode(ast.Inits[i], tabs + 1, w)
			}
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Var Declaration Flags:: %s\n", ast.ClassFlags.String())
		case *FuncDecl:
			fmt.Fprintf(w, "Func Declaration Type\n")
			PrintNode(ast.RetType, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Func Declaration Name\n")
			PrintNode(ast.Ident, tabs + 1, w)
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Func Declaration Flags:: %s\n", ast.ClassFlags.String())
			if len(ast.Params) > 0 {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Func Declaration Params\n")
				for i := range ast.Params {
					PrintNode(ast.Params[i], tabs + 1, w)
				}
			}
			if ast.Body != nil {
				printTabs(c, tabs, w)
				fmt.Fprintf(w, "Func Declaration Body\n")
				PrintNode(ast.Body, tabs + 1, w)
			}
		case *TypeDecl:
			fmt.Fprintf(w, "Type Declaration\n")
			PrintNode(ast.Type, tabs + 1, w)
		case *Plugin:
			fmt.Fprintf(w, "Plugin File\n")
			for i := range ast.Decls {
				PrintNode(ast.Decls[i], tabs + 1, w)
			}
		default:
			fmt.Fprintf(w, "default :: %T\n", ast)
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
		case *TypeSpec:
			Walk(ast.Type, visitor)
		case *EnumSpec:
			Walk(ast.Ident, visitor)
			Walk(ast.Step, visitor)
			for i := range ast.Names {
				Walk(ast.Names[i], visitor)
			}
			for i := range ast.Values {
				Walk(ast.Values[i], visitor)
			}
		case *StructSpec:
			Walk(ast.Ident, visitor)
			for i := range ast.Fields {
				Walk(ast.Fields[i], visitor)
			}
			for i := range ast.Methods {
				Walk(ast.Methods[i], visitor)
			}
		case *UsingSpec:
			Walk(ast.Namespace, visitor)
		case *SignatureSpec:
			Walk(ast.Type, visitor)
			for i := range ast.Params {
				Walk(ast.Params[i], visitor)
			}
		case *TypeDefSpec:
			Walk(ast.Ident, visitor)
			Walk(ast.Sig, visitor)
		case *TypeSetSpec:
			Walk(ast.Ident, visitor)
			for i := range ast.Signatures {
				Walk(ast.Signatures[i], visitor)
			}
		case *VarDecl:
			Walk(ast.Type, visitor)
			for i := range ast.Names {
				if ast.Names[i] != nil {
					Walk(ast.Names[i], visitor)
				}
			}
			for i := range ast.Dims {
				if ast.Dims[i] != nil {
					for _, dim := range ast.Dims[i] {
						Walk(dim, visitor)
					}
				}
			}
			for i := range ast.Inits {
				if ast.Inits[i] != nil {
					Walk(ast.Inits[i], visitor)
				}
			}
		case *FuncDecl:
			Walk(ast.RetType, visitor)
			Walk(ast.Ident, visitor)
			for i := range ast.Params {
				Walk(ast.Params[i], visitor)
			}
			if ast.Body != nil {
				Walk(ast.Body, visitor)
			}
		case *TypeDecl:
			Walk(ast.Type, visitor)
		case *Plugin:
			for i := range ast.Decls {
				Walk(ast.Decls[i], visitor)
			}
	}
}