SuperStrict
Framework BRL.StandardIO ' this is a comment
Include "test_include.bmx"


Local x:T = New T


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
r = a<(b)>(c)   ' comparisons (future: generic function call)
g = (a<b>)(c)   ' generic function call (before: error)
r = (a)<b>(c)   ' comparisons
r = a<b And c>(d)   ' comparisons
r = a<b & c>(d)   ' comparisons (parsed as "a < (b & c) > d") (future: generic function call with the type argument "b & c"
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

If a < b & c > d
While a < b & c > d
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







