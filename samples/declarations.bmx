SuperStrict
Framework BRL.Blitz

Type T Implements I'<S<S>>
	Type U
	End Type
	Function F()
		Function F2()
			Type UU End Type
		End Function
	End Function
End Type

Type T3 Extends T2 End Type
Type T2 Extends T3 End Type

Interface I'<TParam>
End Interface

Struct S
End Struct

Struct S2
End Struct

Rem
Type T1 Extends T2 End Type
Type T2 Implements I2 End Type
Type T3 Extends T2 End Type


Interface I1 End Interface
Interface I2 Extends I1, I3 End Interface
Interface I3 End Interface
End Rem
Rem
Type N1 Extends N2.M2
	Type M1
	End Type
End Type
Type N2
	Type M2 Extends N3
	End Type
End Type
Type N3
	Type M3
		Type O3 Extends M3
		End Type
	End Type
End Type
End Rem

Rem
resolve type declarations (may have cycles inside the same compilation unit -> error)

resolve callable declarations (no inference involved)

resolve variable declarations (inference may have cycles)
-> enqueue like types? or only allow := ininitalizers to refer to declarations further "up" in the code?
End Rem

Rem
TODO: test these:

class A : I<X<Y<S>>> {}
class B : I<Y<X<S>>> {}

interface I<T> {}

class X<T> {}
class Y<T> {}

class S {}

interface IU<T> where T : IU<SS> {}
interface IV<T> where T : IW<SS> {}
interface IW<T> where T : IV<SS> {}

class SS : IU<SS>, IV<SS>, IW<SS> {}

public interface Outer<T> where T : Outer<T>.Inner {
	public interface Inner {}
}
End Rem

Rem
Function F(n:Int = 3)
	Try
		For Local i:Int = 1 Until 3
			Print n + 1.0:Double + "!"
		Next
	Catch e:TBlitzException
	End Try
End Function

Type T
	Method New()
		Local l:Int
	End Method
	Function A() Abstract
End Type


Enum E:Int
	A B
	C
End Enum

Interface I Extends II
	Function F()
	Method M()
	Method M(s:String)
	Method M:String(s:String, i:Int)
End Interface
End Rem

'Type T Extends Object Implements ITest
'	Function F() End Function
'End Type