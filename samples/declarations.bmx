'SuperStrict
'Framework BRL.Blitz

'T.F 1, 2, 3
'T.F(1, 2, 3).M

Rem
Type A End Type
Type B
	'Function F()
		Type C Extends D End Type
		Type D Extends A End Type
	'End Function
End Type
End Rem

Rem
Interface A Extends C.E, C.D
	Interface B End Interface
End Interface
Interface C Extends A
	Interface D
		Interface E End Interface
	End Interface
End Interface
End Rem
Rem
Type E Extends B.X End Type
Type A Extends B Implements I End Type
Type B Extends C.D End Type
Type C Extends CC1 Implements CC2 End Type
Type CC1
	Type D End Type
End Type
Interface CC2
	Type D End Type
End Interface
End Rem
Rem
Interface A
	Interface B Extends A
		Interface C Extends A, B
			Interface D Extends A, B, C
				Interface E Extends A, B, C, D
					Type F End Type
				End Interface
			End Interface
		End Interface
	End Interface
End Interface
Type X Extends A.B.C.D.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.D.C.B ..
	.B.C.D.E.D.C.B.C.D.E.D.C.B.C.D.E.D.C.B.C.D.E.D.C.B.C.D.E.D.C.B.C.D.E.D.C.B.C.D.E.D.C.B.C.D ..
	.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.E.F
End Type
End Rem
Rem
Interface A Extends B, C.CC End Interface
Interface B End Interface
'Interface B End Interface ' duplicate declaration test
Interface C Extends C.CC Interface CC End Interface End Interface
Type T Implements A, C End Type
End Rem
Rem
Type A Extends B.C
	Type V Extends B.C End Type
End Type

Type B Extends D End Type

Type D Extends E.F
	Type C Extends A.V End Type
End Type

Type E Extends EE End Type

Type EE
	Type F End Type
End Type
End Rem
Rem
Type Q Implements I, I End Type
Type R Extends R End Type

Type TT Extends T.U End Type
Type T Extends TT.V Implements I'<S<S>>
'Type T Extends T.U Implements I'<S<S>>
'Type T Implements I'<S<S>>
	Type U
		Type V End Type
	End Type
	'Function F()
	'	Function F2()
	'		Type UU End Type
	'	End Function
	'End Function
End Type
Type sd Extends T End Type
Type sdf Extends T.U End Type
Type TTT Extends TT.V End Type

Type T3 Extends T2 End Type
Type T2 Extends T3 End Type

Interface I'<TParam>
End Interface

Struct S
End Struct

Struct S2
End Struct
End Rem

' TODO: allow parsing:
'Type OG<T> Type IG End Type End Type
'Type IGS Extends OG<Int>.IG End Type


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
- resolve type declarations (may have cycles inside the same compilation unit -> error)
- resolve type member declarations
	- resolve callables
		- needs all types to be available for lookup (no inference)
	- resolve variables
		- needs all types to be available for lookup
		- if inference from initializer: needs all type members it refers to to be available
- resolve code in functions/methods/main
	- resolve local callable declarations (no inference)
		- this includes closures - captures variables are not relevant to the signature
		- maybe closures should depend on code order though to ensure that captured variables are
		  initialized before use (code order: captured vars -> closure decl -> reference to the closure)
	- resolve globals/consts (not order-dependent, inference may have cycles)
	- resolve locals (inference may not have cycles)
-> enqueue like types? or only allow := initalizers to refer to declarations further "up" in the code?
End Rem

Rem
TODO: test these:

interface I<T> {}

class R<T> : I<R<T>> where T : R<T> {}

class A : I<X<Y<S>>> {}
class B : I<Y<X<S>>> {}
class X<T> {}
class Y<T> {}
struct S {}

interface IU<T> where T : IU<SS> {}
interface IV<T> where T : IW<SS> {}
interface IW<T> where T : IV<SS> {}
class SS : IU<SS>, IV<SS>, IW<SS> {}

public interface Outer<T> where T : Outer<T>.Inner {
	public interface Inner {}
}

interface OOO<T> where T : III {}
interface III : OOO<III> {}

class CCC : DDD<CCC>{} 
class DDD<T>{}

interface V<T> where T : W<T> {}
interface W<T> : V<T> where T : W<T> {}

//class RR<T> : T {} // <- NOT allowed

class Outer1 {public class Inner1 : Outer1 {}}
//class Outer2 : Outer2.Inner2 {public class Inner2 {}} // <- NOT allowed
class Outer3 : I<Outer3.Inner3> {public class Inner3 {}}
class Outer4<T> where T : Outer4<T>.Inner4<Outer4<T>> {public class Inner4<T> {}}

public class OO { public class II : OO {} }
public class NN : OO.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II.II {}

public class RR<T> : I<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<RR<T>>>>>>>>>>>>>>>>>>>> {}
public class RRR<T> : I<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<RRR<T>>>>>>>>>>>>>>>> {}
End Rem



Rem
// unbounded generic nesting - this won't be possible
interface Int
{
	public abstract int Value();
	public static abstract Int Add(int i);
}
class Zero : Int
{
	public int Value() { return 0; }
	public static Int Add(int i)
	{
		if (i == 0) return new Zero(); else return Succ<Zero>.Add(i - 1); 
	}
}
class Succ<T> : Int where T : Int, new()
{
	public int Value() { return 1 + new T().Value(); }
	public static Int Add(int i)
	{
		if (i == 0) return new Succ<T>(); else return Succ<Succ<T>>.Add(i - 1); 
	}
}

public class Program
{
	public static void Main()
	{
		Int i = Zero.Add(10000);
		Console.WriteLine(i.Value());
	}
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