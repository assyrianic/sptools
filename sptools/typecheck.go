package SPTools

import (
	//"io"
	//"fmt"
	///"time"
)


/*
 * Check that identifiers are declared before to be used in computations.
 * Check that reserved keywords are not misused.
 * Check that types are correctly declared, if the language is explicitly typed.
 * Check that computations are type-consistent, wherever possible.
 */

/*
type Type struct {
	FieldsOrParams map[string]Type
	///D Decl
	RefOrArrayOrRet *Type
	Len, Offset, StorageClass int
	Kind TypeKind
	HasFields bool
}

type Scope struct {
	Types, Syms map[*Name]Node
	Kids []*Scope
	Parent *Scope
}


func NewScope() *Scope {
	return &Scope{ Types: make(map[*Name]Node), Syms: make(map[*Name]Node) }
}
*/

type (
	Type interface {
		aType()
		Sizeof() int
		Alignof() int
	}
	
	IntType struct { _type }
	FloatType struct { _type }
	AnyType struct { _type }
	
	RefType struct {
		Elem Type
		_type
	}
	
	ArrayType struct {
		Elem Type
		Len []int // 1 or more values of 0 means unknown size.
		Dynamic bool // dynamically stack allocated.
		_type
	}
	
	FuncType struct {
		Params map[string]Type // empty/nill if no params.
		Ret Type // nil == 'void'
	}
)

type _type struct {
	size, align int
	alias Type // Is the type an alias of another? nil alias means treat as separate type.
}

func (*_type) aType() {}
func (t *_type) Sizeof() int { return t.size }
func (t *_type) Alignof() int { return t.align }
func (t *_type) Alias() Type { return t.alias }


var DefaultIntType = &IntType{ _type{ size: 4, align: 4, alias: nil } }
var DefaultBoolType = &IntType{ _type{ alias: DefaultIntType } }
var DefaultCharType = &IntType{ _type{ size: 1, align: 1, alias: nil } }
var DefaultFloatType = &FloatType{ _type{ size: 4, align: 4, alias: nil } }
var DefaultHandleType = &IntType{ _type{ size: 4, align: 4, alias: nil } }


/*
 * 1. Two expressions are convertible when their reduced forms are the same. E.g 2 + 2 is convertible to 4
 * 2. Two expressions are coercible when you can safely cast one to the other. E.g 22 : int32 might be coercible to 22 : int64
 */
/*
func IsTypeCoercible(a, b Type) bool {
	switch typ := a.(type) {
	case *IntType:
		
	}
}
*/


func TypeCheckExpr(e Expr) Type {
	switch ast := e.(type) {
	case *BasicLit:
		switch ast.Kind {
		case IntLit:
			return DefaultIntType
		case BoolLit:
			return DefaultBoolType
		case CharLit:
			return DefaultIntType
		case FloatLit:
			return DefaultFloatType
		case StringLit:
			return &ArrayType{ Elem: DefaultCharType, Len: []int{ len(ast.Value) + 1 } }
		}
	case *Name, *ThisExpr:
		// returning type integer for now.
		// ast.Value
		return DefaultIntType
	case *NullExpr:
		return DefaultIntType
	/*
	case *FieldExpr:
		// check ast.X, check if X has the field as well.
		return TypeCheckExpr(ast.Sel)
	case *UnaryExpr:
		// make sure ast.X is a specific type or object.
		// TypeCheckExpr(ast.X)
		switch ast.Kind {
		case TKIncr, TKDecr, TKNot, TKCompl, TKSub, TKSizeof:
			return TypeInt
		case TKNew:
			return TypeInvalid
		}
	case *ViewAsExpr:
		// type casting.
		// check if type exists.
		// ast.Type.TokenName.Lexeme
		TypeCheckExpr(ast.X) // then gotta check if convertable.
	case *BinExpr:
		if TypeCheckExpr(ast.L) != TypeCheckExpr(ast.R) {}
		///switch ast.Kind {}
	*/
	}
	return DefaultIntType
}