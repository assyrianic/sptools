# Pre SM 1.7's SourcePawn Grammar according to the parsing used by sptools:

## Declarations
```ebnf

```

## Specifications
```ebnf

```

## Statements
```ebnf

```

## Expressions
```ebnf
Expr = AssignExpr *( ',' AssignExpr ) .
AssignExpr = SubMainExpr *( '['+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '<<' | '>>' | '>>>' ] =' SubMainExpr ) .
SubMainExpr = LogicalOrExpr [ TernaryExpr ] .
TernaryExpr = '?' SubMainExpr ':' Expr .

LogicalOrExpr = LogicalAndExpr *( '||' LogicalAndExpr ) .
LogicalAndExpr = EqualExpr *( '&&' EqualExpr ) .

EqualExpr = RelExpr *( ( '==' | '!=' ) RelExpr ) .
RelExpr = BitOrExpr *( ( '<[=]' | '>[=]' ) BitOrExpr ) .

BitOrExpr = BitXorExpr *( '|' BitXorExpr ) .
BitXorExpr = BitAndExpr *( '^' BitAndExpr ) .
BitAndExpr = ShiftExpr *( '&' ShiftExpr ) .
ShiftExpr = AddExpr *( ( '<<' | '>>' | '>>>' ) AddExpr ) .

AddExpr = MulExpr *( ( '+' | '-' ) MulExpr ) .
MulExpr = PrefixExpr *( ( '*' | '/' | '%' ) PrefixExpr ) .

PrefixExpr = *( '!' | '~' | '-' | '++' | '--' | 'sizeof' | 'defined' | 'new' ) PostfixExpr .

TypeExpr = [ '<' ] ( ident | '[u]int[8|16|32|64|n]' | 'float' | 'char' | 'bool' ) [ '>' ] .
ViewAsExpr = TypeExpr '(' MainExpr ')' .

NamedArgExpr = '.' AssignExpr .
ExprList = START ListedExpr *( SEP ListedExpr ) END .
ListedExpr = NamedArgExpr | AssignExpr .

PostfixExpr = Primary *( '.' identifier | '[' Expr ']' | '(' [ ExprList ] ')' | '::' identifier | '++' | '--' ) .

BoolLit = 'true' | 'false' .
BasicLit = int_lit | rune_lit | string_lit .
BracketExpr = '{' ExprList '}' .
Primary = BasicLit | identifier | 'operator' op | BoolLit | 'this' | 'null' | '...' | '(' Expr ')' | BracketExpr .
```
