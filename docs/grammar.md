# SourcePawn's Grammar according to the parsing used by sptools:

## Declarations
```ebnf
Plugin = +TopDecl .
TopDecl = FuncDecl | TypeDecl | VarDecl | StaticAssert .

StaticAssert = 'static_assert' '(' Expr [ ',' Expr ] ')' ';' .
VarDecl  = VarOrFuncSpec VarDeclarator .
FuncDecl = VarOrFuncSpec FuncDeclarator .
TypeDecl = EnumSpec | StructSpec | UsingSpec | TypeSetSpec | TypeDefSpec | MethodMapSpec .

VarDeclarator = Ident [ IndexExpr ] [ Initializer ] *( ',' VarDeclarator ) .
Initializer = '=' SubMainExpr | '{' Expr [ ',' ( '...' | *Expr ) ] '}' .

ParamList = '(' *VarDecl ')' .
FuncDeclarator = Ident ParamList ( Initializer | BlockStmt | ';' ) .

StorageClass = 'native' | 'forward' | 'const' | 'static' | 'stock' | 'public' | 'private' | 'protected' | 'readonly' | 'sealed' | 'virtual' .
AbstractDecl = Type [ +'[]' | '&' ] .

VarOrFuncSpec = *StorageClass AbstractDecl .
SignatureSpec = 'function' AbstractDecl ParamsList .
```

## Specifications
```ebnf
EnumSpec = 'enum' [ ident [ ':' ] [ '(' operator PrimaryExpr ')' ] ] '{' +EnumEntry '}' [ ';' ] .
EnumEntry = Ident [ '=' Expr ] .

StructSpec = [ 'enum' ] 'struct' Ident '{' *Field '}' [ ';' ] .
Field = VarDecl ';' | FuncDecl .

UsingSpec = 'using' Expr ';' .

TypeSetSpec = 'typeset' Ident '{' *( SignatureSpec ';' ) '}' [ ';' ] .
TypeDefSpec = 'typedef' Ident '=' SignatureSpec ';' .

MethodMapSpec = 'methodmap' Ident [ '__nullable__' ] [ '<' TypeExpr ] '{' [ MethodCtor ] *MethodMapEntry '}' [ ';' ] .
MethodCtor = 'public' [ 'native' ] Ident ParamList ( BlockStmt | ';' ) .
MethodMapEntry = MethodMapProp | FuncDecl .
MethodMapProp = 'property' TypeExpr Ident '{' PropGetter [ PropSetter ] | PropSetter '}' .

PropGetter = 'public' [ 'native' ] 'get' '(' ')' ( BlockStmt | ';' ) .
PropSetter = 'public' [ 'native' ] 'set' ParamList ( BlockStmt | ';' ) .
```

## Statements
```ebnf
BlockStmt = '{' *Statement '}' .
Statement = IfStmt | DoStmt | WhileStmt | ForStmt | SwitchStmt | BlockStmt | RetStmt | AssertStmt | StaticAssertStmt | DeclStmt | DeleteStmt | ExprStmt .

RetStmt = 'return' [ Expr ] ';' .
StaticAssertStmt = StaticAssert .
AssertStmt = 'assert' Expr ';' .
DeleteStmt = 'delete' Expr ';' .

DoStmt = 'do' Statement 'while' '(' Expr ')' ';' .
WhileStmt = 'while' '(' Expr ')' Statement .

IfStmt = 'if' '(' Expr ')' Statement [ 'else' Statement ] .

ForStmt = 'for' '(' [ VarDecl | Expr ] ';' [ Expr ] ';' [ Expr ] ')' Statement .

SwitchStmt = 'switch' '(' Expr ')' '{' *CaseClause '}' .
CaseClause = 'case' ExprList ':' Statement | 'default' ':' Statement .

DeclStmt = VarDecl ';' .
ExprStmt = Expr ';' .
```

## Expressions
```ebnf
Expr = AssignExpr *( ',' AssignExpr ) .
AssignExpr = SubMainExpr *( '['+' | '-' | '*' | '/' | '%' | '&' | '&~' | '|' | '^' | '<<' | '>>' | '>>>' ] =' SubMainExpr ) .
SubMainExpr = LogicalOrExpr [ TernaryExpr ] .
TernaryExpr = '?' SubMainExpr ':' Expr .

LogicalOrExpr = LogicalAndExpr *( '||' LogicalAndExpr ) .
LogicalAndExpr = EqualExpr *( '&&' EqualExpr ) .

EqualExpr = RelExpr *( ( '==' | '!=' ) RelExpr ) .
RelExpr = BitOrExpr *( ( '<[=]' | '>[=]' ) BitOrExpr ) .

BitOrExpr = BitXorExpr *( '|' BitXorExpr ) .
BitXorExpr = BitAndExpr *( '^' BitAndExpr ) .
BitAndExpr = ShiftExpr *( ('&' | '&~') ShiftExpr ) .
ShiftExpr = AddExpr *( ( '<<' | '>>' | '>>>' ) AddExpr ) .

AddExpr = MulExpr *( ( '+' | '-' ) MulExpr ) .
MulExpr = PrefixExpr *( ( '*' | '/' | '%' ) PrefixExpr ) .

PrefixExpr = *( '!' | '~' | '-' | '++' | '--' | 'sizeof' | 'defined' | 'new' ) PostfixExpr .

TypeExpr = ( ident | '[u]int[8|16|32|64|n]' | 'float' | 'char' | 'bool' ) .
ViewAsExpr = 'view_as' '<' TypeExpr '>' '(' MainExpr ')' .

NamedArgExpr = '.' AssignExpr .
ExprList = START ListedExpr *( SEP ListedExpr ) END .
ListedExpr = NamedArgExpr | AssignExpr .

PostfixExpr = (ViewAsExpr | Primary) *( '.' identifier | '[' Expr ']' | '(' [ ExprList ] ')' | '::' identifier | '++' | '--' ) .

BoolLit = 'true' | 'false' .
BasicLit = int_lit | rune_lit | string_lit .
BracketExpr = '{' ExprList '}' .
[SPTOOLS EXTENSION] FuncLit = SignatureSpec BlockStmt .
Primary = BasicLit | identifier | 'operator' op | BoolLit | 'this' | 'null' | '...' | '(' Expr ')' | BracketExpr | FuncLit .

int_lit = [0-9]+ | ('0b' | '0B') [01]+ | ('0o' | '0O') [0-7]+ | ('0x' | '0X') [0-9, a-f, A-F]+ .
rune_lit = "'" *( letter | escape_character ) "'" .
string_lit = "'''" *(all_characters) "'''" | '"' *( letter | escape_character ) '"' .
identifier = ( '_' | [A-Z] | [a-z] | '$' ) *( '_' | [A-Z] | [a-z] | '$' | [0-9] ) .
```