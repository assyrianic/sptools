package SPTools

/*
import (
	"io"
	"fmt"
	///"time"
)
*/


type TypeKind uint8
const (
	TypeInvalid = TypeKind(0) // also acts as void/unit.
	TypeInt
	TypeString
	TypeBool
	TypeFloat
	TypeFunc
	MaxTypes
)