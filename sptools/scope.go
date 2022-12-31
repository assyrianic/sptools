package SPTools

import (
	"os"
	"fmt"
	///"time"
)


/*
 * Okay so now here's what you do. You need to work out what the type of every expression is. For simple expressions that is a simple operation, 1 is probably an integer, "foo" is probably a string - depends on your language rules.
 *
 * Variables require some tracking, if you see x then you need to lookup the definition of x and see what type it was declared with. That's the type of whatever value is inside of x and therefore the type of whatever value you get by reading the variable x.
 *
 * Finally, you've got expressions like function calls or operators. foo(a, b) and those work by getting the type of all of the sub-expressions (so get the type of a and b separately) then looking up an appropriate definition of foo and the return type of that function is the type of the expression.
 *
 * Several things can go wrong. Simplest example is new FOO:x; new BAR:y; ...; x = y you get to invent the rules on what happens when users write this code but for the sake of example, I'm going to say that this is an error because you're only allowed to assign values of type FOO to variables of type FOO. The way this sort of thing works is that you lookup the type of x and the type of y and figure out if the assignment is allowed to happen.
 *
 * So in this example we lookup x see that it has type FOO, lookup y see that it has type BAR, and oopsie you're not allowed to assign BAR to a FOO. In the compiler you probably want these types to be represented by some structure that gives your type checker all the information it needs to make these decisions, like you mentioned before about some difference in behaviour for named enums vs anonymous enums you would probably want to have a field on the structure your compiler uses to represent the type that tells it if it's an anonymous enum.
 *
 * This is also how you handle things like implicit conversions or whatever else. Let's say that you decide that `new int:a; new float:b; ...; a = b` means to round b down to the nearest integer. When you're processing that assignment statement/expression you see that b has type float and a has type int so you know that you should emit a rounding operation as well as a value copy.
 */

/*
 * First collect type and function names.
 * Then verify their usages in the correct areas:
 *     * Functions -> function calling.
 *     * 
 */

type Scope struct {
	MsgSpan
	Syms    map[Node]Decl
	Parent *Scope
}

func MakeScope(ms MsgSpan, parent *Scope) *Scope {
	return &Scope{ MsgSpan: ms, Syms: make(map[Node]Decl) }
}

func (s *Scope) FindSym(name Node) Decl {
	sym, has := s.Syms[name]
	if !has {
		if s.Parent != nil {
			return s.Parent.FindSym(name)
		}
		return nil
	}
	return sym
}

func (s *Scope) DoMessage(n Node, msgtype, color, msg string, args ...any) string {
	t := n.Tok()
	report := s.MsgSpan.Report(msgtype, "", color, msg, *t.Path, &t.Span.LineStart, &t.Span.ColStart, args...)
	s.MsgSpan.PurgeNotes()
	return report
}


func (s *Scope) CollectFuncAndTypeNames(n Node) {
	if n==nil {
		return
	}
	
	if IsPluginNode(n) {
		plugin := n.(*Plugin)
		for i := range plugin.Decls {
			s.VisitDecl(plugin.Decls[i])
		}
	} else if IsDeclNode(n) {
		s.VisitDecl(n.(Decl))
	}
}

func (s *Scope) VisitDecl(d Decl) {
	if d==nil {
		return
	}
	
	switch ast := d.(type) {
	case *FuncDecl, *TypeDecl:
		if s.FindSym(ast) != nil {
			// conflicting func/type names.
			return
		}
		s.Syms[ast.Ident] = ast
	}
}

func (s *Scope) VisitStmt(s Stmt) {
	if s==nil {
		return
	}
	
	switch ast := s.(type) {
	case *BlockStmt:
	case *WhileStmt:
	case *IfStmt:
	case *FlowStmt:
	case *RetStmt:
	case *ExprStmt:
	case *BadStmt:
	}
}

func (s *Scope) VisitExpr(e Expr) {
	if e==nil {
		return
	}
	
	switch ast := e.(type) {
	case *BasicLit:
	case *BracketExpr:
	case *NullExpr:
	case *ThisExpr:
	case *Name:
	case *UnaryExpr:
	case *IndexExpr:
	case *ViewAsExpr:
	case *BinExpr:
	case *ChainExpr:
	case *TernaryExpr:
	case *CommaExpr:
	case *FuncLit:
	}
