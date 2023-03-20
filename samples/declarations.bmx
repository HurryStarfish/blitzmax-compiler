SuperStrict
Framework BRL.Blitz

Function F(n:Int = 3)
	Try
		For Local i:Int = 1 Until 3
			Print n + 1.0:Double + "!"
		Next
	Catch e:TBlitzException
	End Try
End Function

Type T
	'Method New()
	'	Local l:Int
	'End Method
	Function A Abstract
End Type


Enum E:Int
	A B
	C
End Enum

Interface I Extends II
	Function F()
	Method M()
End Interface


'Type T Extends Object Implements ITest
'	Function F() End Function
'End Type