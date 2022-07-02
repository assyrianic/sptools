package SPTools

import (
	"io"
	"fmt"
	///"time"
	"strings"
)


type (
	Pos struct {
		Line, Col uint32
		Path     *string
		Tok      *Token
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
func (*node) aNode() {}

func copyPosToNode(n *node, t Token) {
	n.pos.Line, n.pos.Col = t.Line, t.Col
	n.pos.Path, n.pos.Tok = t.Path, &t
}


// top-level plugin.
type Plugin struct {
	Decls []Decl
	node
}


type StorageClassFlags uint16
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
	MaxStorageClasses
)

var StorageClassToString = [...]string{
	IsPublic: "public",
	IsConst: "const",
	IsNative: "native",
	IsForward: "forward",
	IsStatic: "static",
	IsStock: "stock",
	IsPrivate: "private",
	IsProtected: "protected",
	IsReadOnly: "readonly",
	IsSealed: "sealed",
	IsVirtual: "virtual",
}

func (sc StorageClassFlags) String() string {
	var sb strings.Builder
	for flag := IsPublic; sc != 0 && flag < MaxStorageClasses; flag <<= 1 {
		if sc & flag > 0 {
			if sb.Len() > 0 {
				sb.WriteString(" ")
			}
			sb.WriteString(StorageClassToString[flag])
			sc &^= flag
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
		// valid dim index but empty means [] auto counting.
		// nil index if there was no initializer.
		ClassFlags StorageClassFlags
		decl
	}
	
	// class type name() {}
	// class type name();
	// class type name1() = name2;
	FuncDecl struct {
		RetType Spec // *TypeSpec
		Ident Expr
		Params []Decl // []*VarDecl, *BadDecl if error.
		Body Node // Expr if alias, Stmt if body, nil if ';'.
		ClassFlags StorageClassFlags
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
		Methods []Spec // []*MethodMapMethodSpec
		Nullable bool
		spec
	}
	
	/* property Type name {
	 *    public get() {}
	 *    public set(Type param) {}
	 *    
	 *    public native get();
	 *    public native set(Type param);
	 * }
	 */
	MethodMapPropSpec struct {
		Type, Ident Expr
		SetterParams []Decl
		GetterBlock, SetterBlock Stmt
		GetterClass, SetterClass StorageClassFlags
		spec
	}
	// public Type name(params) {}
	// public native Type name(params);
	MethodMapMethodSpec struct {
		Impl Decl
		IsCtor bool
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
	
	// { a, b, c }
	BracketExpr struct {
		Exprs []Expr
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


func printTabs(c rune, tabs int, w io.Writer) {
	for i := 0; i < tabs; i++ {
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
		fmt.Fprintf(w, "Bad/Errored Stmt Node:: Line: %v | Col: %v | Path: %q | Tok: %q\n", ast.node.pos.Line, ast.node.pos.Col, *ast.node.pos.Path, ast.node.pos.Tok)
	case *BadExpr:
		fmt.Fprintf(w, "Bad/Errored Expr Node:: Line: %v | Col: %v | Path: %q | Tok: %q\n", ast.node.pos.Line, ast.node.pos.Col, *ast.node.pos.Path, ast.node.pos.Tok)
	case *BadSpec:
		fmt.Fprintf(w, "Bad/Errored Spec Node:: Line: %v | Col: %v | Path: %q | Tok: %q\n", ast.node.pos.Line, ast.node.pos.Col, *ast.node.pos.Path, ast.node.pos.Tok)
	case *BadDecl:
		fmt.Fprintf(w, "Bad/Errored Decl Node:: Line: %v | Col: %v | Path: %q | Tok: %q\n", ast.node.pos.Line, ast.node.pos.Col, *ast.node.pos.Path, ast.node.pos.Tok)
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
		fmt.Fprintf(w, "Typed Expr - Kind: %q\n", ast.Tok.String())
	case *CommaExpr:
		fmt.Fprintf(w, "Comma Expr\n")
		for i := range ast.Exprs {
			PrintNode(ast.Exprs[i], tabs + 1, w)
		}
	case *BracketExpr:
		fmt.Fprintf(w, "Bracket Expr\n")
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
	case *MethodMapSpec:
		fmt.Fprintf(w, "Methodmap Specification Ident\n")
		PrintNode(ast.Ident, tabs + 1, w)
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Specification:: Is Nullable? %t\n", ast.Nullable)
		if ast.Parent != nil {
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Methodmap Specification Derived-From\n")
			PrintNode(ast.Parent, tabs + 1, w)
		}
		if len(ast.Props) > 0 {
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Methodmap Specification Props\n")
			for i := range ast.Props {
				PrintNode(ast.Props[i], tabs + 1, w)
			}
		}
		if len(ast.Methods) > 0 {
			printTabs(c, tabs, w)
			fmt.Fprintf(w, "Methodmap Specification Methods\n")
			for i := range ast.Methods {
				PrintNode(ast.Methods[i], tabs + 1, w)
			}
		}
	case *MethodMapPropSpec:
		fmt.Fprintf(w, "Methodmap Property Specification Type\n")
		PrintNode(ast.Type, tabs + 1, w)
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Property Ident\n")
		PrintNode(ast.Ident, tabs + 1, w)
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Property Get Storage Class: '%s'\n", ast.GetterClass.String())
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Property Get Block\n")
		PrintNode(ast.GetterBlock, tabs + 1, w)
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Property Set Storage Class: '%s'\n", ast.SetterClass.String())
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Property Set Params\n")
		for i := range ast.SetterParams {
			PrintNode(ast.SetterParams[i], tabs + 1, w)
		}
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Property Set Block\n")
		PrintNode(ast.SetterBlock, tabs + 1, w)
	case *MethodMapMethodSpec:
		fmt.Fprintf(w, "Methodmap Method Specification\n")
		printTabs(c, tabs, w)
		fmt.Fprintf(w, "Methodmap Method Specification:: Is Constructor? %t\n", ast.IsCtor)
		PrintNode(ast.Impl, tabs + 1, w)
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
	case *BracketExpr:
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
	case *MethodMapSpec:
		Walk(ast.Ident, visitor)
		Walk(ast.Parent, visitor)
		for i := range ast.Props {
			Walk(ast.Props[i], visitor)
		}
		for i := range ast.Methods {
			Walk(ast.Methods[i], visitor)
		}
	case *MethodMapPropSpec:
		Walk(ast.Type, visitor)
		Walk(ast.Ident, visitor)
		Walk(ast.GetterBlock, visitor)
		for i := range ast.SetterParams {
			Walk(ast.SetterParams[i], visitor)
		}
		Walk(ast.SetterBlock, visitor)
	case *MethodMapMethodSpec:
		Walk(ast.Impl, visitor)
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
