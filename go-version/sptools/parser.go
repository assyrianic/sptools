package SPTools

import (
	"os"
)


type (
	SPParser struct {
		tokens  []SPToken
		idx, errs int
	}
	SPPos struct {
		Line, Col int
	}
	SPNode interface {
		// line & col.
		Pos() SPPos
		aSPNode()
	}
)

type spNode struct { pos SPPos }
func (n *spNode) Pos() SPPos { return n.pos }
func (*spNode) aSPNode()       {}


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
		SPNode
		aExpr()
	}
	
	BadExpr struct {
		expr
	}
	
	// id.name
	FieldExpr struct {
		X    Expr
		Sel *BasicLit
		expr
	}
	
	// a[i]
	IndexExpr struct {
		X     Expr
		Index Expr
		expr
	}
	
	// f(a,b,...z);
	CallExpr struct {
		Func      Expr
		ArgList []Expr // nil means no arguments
		expr
	}
	
	ThisExpr struct {
		expr
	}
	
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
type expr struct{ spNode }
func (*expr) aExpr() {}



// PostFix = Primary *( '.' identifier | '[' Expr ']' | '(' [ ExprList ] ')' | '::' identifier ) .
func (parser *SPParser) PostFixExpr() Expr {
	primary := parser.PrimaryExpr()
	for t := parser.tokens[parser.idx]; t.Kind==SPTKDot || t.Kind==SPTKLBrack || t.Kind==SPTKLParen || t.Kind==SPTK2Colons; t = parser.tokens[parser.idx] {
		switch t.Kind {
			case SPTKDot:
			case SPTKLBrack:
			case SPTKLParen:
			case SPTK2Colons:
		}
	}
	return primary
}

// Primary = int_lit | rune_lit | string_lit | identifier | 'this' | '(' Expr ')' .
func (parser *SPParser) PrimaryExpr() Expr {
	var ret_expr Expr
	switch prim := parser.tokens[parser.idx]; prim.Kind {
		//case SPTKLParen:
			// nested expression
		case SPTKIdent:
			iden := new(Name)
			iden.Value = prim.Lexeme
			iden.spNode.pos = SPPos{ Line: prim.Line, Col: prim.Col }
			ret_expr = iden
		case SPTKIntLit:
			num := new(BasicLit)
			num.Value = prim.Lexeme
			num.Kind = IntLit
			num.spNode.pos = SPPos{ Line: prim.Line, Col: prim.Col }
			ret_expr = num
		case SPTKFloatLit:
			num := new(BasicLit)
			num.Value = prim.Lexeme
			num.Kind = FloatLit
			num.spNode.pos = SPPos{ Line: prim.Line, Col: prim.Col }
			ret_expr = num
		case SPTKStrLit:
			str := new(BasicLit)
			str.Value = prim.Lexeme
			str.Kind = StringLit
			str.spNode.pos = SPPos{ Line: prim.Line, Col: prim.Col }
			ret_expr = str
		case SPTKCharLit:
			str := new(BasicLit)
			str.Value = prim.Lexeme
			str.Kind = CharLit
			str.spNode.pos = SPPos{ Line: prim.Line, Col: prim.Col }
			ret_expr = str
		case SPTKThis:
			this := new(ThisExpr)
			this.spNode.pos = SPPos{ Line: prim.Line, Col: prim.Col }
			ret_expr = this
		default:
			writeMsg(&parser.errs, os.Stdout, *prim.Path, "syntax error", COLOR_RED, &prim.Line, &prim.Col, "bad primary expression")
			ret_expr = nil
	}
	if ret_expr != nil {
		parser.idx++
	}
	return ret_expr
}