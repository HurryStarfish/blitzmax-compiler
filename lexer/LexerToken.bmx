SuperStrict
Import "TokenKind.bmx"
Import "CodeLocation.bmx"



Global EOFToken:TLexerToken = New TLexerToken(Null, TTokenKind.Eof, New SCodeLocation("", 0, 0))

Type TLexerToken Final
	Field ReadOnly value:String
	Field ReadOnly kind:TTokenKind
	Field ReadOnly location:SCodeLocation
	Field ReadOnly missing:Int'Bool
	
	Private
	Method New() End Method
	
	Public
	Method New(value:String, kind:TTokenKind, location:SCodeLocation, missing:Int = False)
		Self.value = value
		Self.kind = kind
		Self.location = location
		Self.missing = missing
	End Method
	
	Method ToString:String() Override
		Function Escape:String(s:String)
			Return s.Replace("~r", "~~r").Replace("~n", "~~n")
		End Function
		Return kind.ToString() + " ~q" + Escape(value) + "~q     (" + location.ToString() + ")"
	End Method
End Type
