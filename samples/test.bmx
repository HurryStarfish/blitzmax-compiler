SuperStrict
Framework BRL.StandardIO ' this is a comment


'Local x:T = New T


'Local m:BRL.Collections.IMap<Int, BRL.Collections.IList<Int>>
'Type TTest<T>
'	Method M<A, B>:R(_:P) Abstract
'End Type

'Local x:TList<Int>
'Local y:Object = STest<Int> Ptr(a)
'Local z:Object = A<B<C>>()

Rem
r = a<b   ' comparison
g = a<b>   ' generic function ptr (before: error)
r = a<b>c   ' comparisons
g = a<b>(c)   ' generic function call (before: comparisons)
g = a<(b%)>(c)   ' generic function call (before: comparisons)
rg = a<(b)>(c)   ' comparisons (future: generic function call)
g = (a<b>)(c)   ' generic function call (before: error)
r = (a+a)<b>(c)   ' comparisons
g = (a)<b>(c)   ' generic function call
g = (a.x)<b>(c)   ' generic function call
g = a.x<b>(c)   '  generic function call
g = (a).x<b>(c)   ' generic function call
g = (a[i]).x<b>(c)   ' generic function call
r = (a.x[i])<b>(c)   ' comparisons
r = a<b And c>(d)   ' comparisons
rg = a<b & c>(d)   ' comparisons (parsed as "a < (b & c) > d") (future: generic function call with the type argument "b & c"
r = a<-b>(d)   ' comparisons, or generic call with an error (currently comparisons)
r = f(a<b,c>d)   ' comparisons
g = f(a<b,c>(d))   ' generic call (before: comparisons)
g = f(a<b,c>.d)   ' generic member access (before: comparisons)
g = f(a<b,c>(.d))   ' generic call (before: comparisons)
r = f(a<b,(c>.d))   ' comparisons
r = a<b(x>(y+z))   ' comparisons


f
f x
f<a>   ' generic call (before: error)
f<a>b   ' generic call (before: error)
f<a>b,c   ' generic call (before: error)

If a < b & c > d End If
While a < b & c > d Wend
'For i = a < b To c > d Next
End Rem
Rem
' specific requirements:
' - "<" can only be part of a generic if it directly follows an identifier (or some specific keywords?)

' - "a<b>(c)" should be a generic call (or cast)
'   - reason: intuitive (looks like the non-generic equivalent with "<b>" added)

' - "a<b>c", *as an expression*, should be comparisons
'   - reason: would always be an error if generic

' - "a<b>", *as an expression*, should be a reference to a generic function
'   - reason: intuitive (looks like the non-generic equivalent with "<b>" added

' - "a<b>c" or "a<b>", *as a statement*, should be a generic call (parentheses are optional)
'   - reason: would always be an error otherwise (a comparison cannot be used as a statement)

' - in order to tell "a<b>" apart from "a<b>c" and "a<b>(c)", at least one token after ">" must be checked
'   - these ones definitely begin a new expression, would not make sense after a generic:
'     - prefix operators (like "Varptr"), excluding the ones that double as binary operators (like "+")
'     - starting tokens of primary expressions (like identifiers), excluding "("
'     - type keywords
'   - these ones definitely end a context or begin a new non-expression, only make sense after a generic:
'     - statement separator
'     - "For", "While", etc. (excluding "If" once if-expressions get added, "Throw" for throw expressions, ...)
'     - "Local", "Function", "Type", etc.
'     - ")", "]"
'     - ","
'   - these ones are definitely a followup to a generic:
'     - "Ptr" (also "Var" in principle, although not legal in expression contexts currently)
'     - binary operators, excluding those that also can be unary
'     - ":" (type assertion - should this even be legal?)
'   - these ones are possibly a followup to a generic, but ambiguous (could also be the start of a comparison rhs):
'     - "(", ".", "["

' - "a<b And c>d" or "a<b And c>(d)" should be comparisons
'   - reason: this is common and shouldnt break

' - "a<b & c>d" or "a<b & c>(d)" should be generics (same with other binary operators that have higher precedence than comparison)
'   - reason: due to their precedence, this would otherwise be parsed as "a < (b & c) > d" (not as "(a < b) & (c > d)"), which is unlikely to be useful
'             making this an error opens up the possibility of making things like "b & c" valid type syntax in the future (union/intersection types)

' general rules:
' - when ambiguous, prefer parsing as generic instead of comparisons
'   - reason: when something gets parsed as generic, a parse as comparisons can easily be forced by adding more () or by using Locals to split the expression into smaller parts - the inverse would be much harder

' - when the code looks like a chain of multiple comparisons ("a < b > c"), always parse as generic (even if that results in an error)
'   - reason: chains like that do not really make sense (and would result in a type error with a proper boolean type), so it should be fine to break/discourage them
'             parsing this as generic even if it causes an error leaves some room for error recovery and future additions to type syntax


'Extern "win32"
'	Function ExtractIconA:Byte Ptr(hInst:Byte Ptr, lpszExeFileName$z, iconIndex:Int)="HICON ExtractIconA(HINSTANCE , LPCSTR , UINT )!"
'	Function ExtractIconW:Byte Ptr(hInst:Byte Ptr, lpszExeFileName$w, iconIndex:Int)="HICON ExtractIconW(HINSTANCE , LPCWSTR , UINT )!"
'End Extern
End Rem

'Local i:Int[] = [1, 2, 3]
Rem
Local arr:Int[,]
'Local arr:Int[,<] ' TODO: error for this


Local a:Int = 3, b:Int = 4

'Local a:Int = a[](1)

'Local a:Int = x[1].y
'Local a:abc = f(+-Object Int[]Ptr[,]Varptr~x[1].y())


'x.y().z


X(a..b    ..
..
,.. 
,    .. ..
c,d..,..)
'! //c


'Print TTest.F()   ' bla


Type TTest
	Field ReadOnly s:String = "asdf" {asdf}
	
	Function Increment:Int(i:Int) Final
		Local j:Int = 1
		Return i + j + 3
	End Function
	
	Method M() Abstract ' bla
	
		Method bloerk(a())Default
			Self=Self
			(1+2).ToString()
		End Method

	Method Operator +:Int(other:Int)
	End Method
End Type


'asdf
'fgh.jkl ue
'aaa ,aaa,
'arrrr[3]
'yo()
Print "blub"
New T
a()[1,2] 3
.x = .2

'x = a(b:Int)(c)

'x = a[](b:Int)(c)
End Rem

'If a Then; b Else c End If

'a = TMyType.SMyStruct Ptr a * b + c

'Print a:Int
'For Local i:Int == 1 To 10 + x
'	X
'Next

'Function F:Int((a:Int)
'	X
'End Function

'For j = EachIn A
'	Y
'Next
'Local a:Int[]=[1,2,3,]
Rem
If x Then
	For y = 1 To 2
		While z
			a
			b
		Forever
		c
	Next
	d
End If

If x Then
	For y = 1 To 2
		While z
			a
			b
		Next
		c
	Next
	d
End If
End Rem

'Local a:Int[,] = New A.B[3,3][9]
'x = A[3] B
'Function F()'(x:i[1] = 1)
'End Function
'a :+ b







