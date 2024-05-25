SuperStrict
Import "TokenKind.bmx"
Import "CodeRange.bmx"
Import "CodeInfo.bmx"



Type TLexerToken Final
	Field ReadOnly value:String
	Field ReadOnly kind:TTokenKind
	Field ReadOnly missing:Int'Bool
	
	Private
	Method New() End Method
	
	Method New(value:String, kind:TTokenKind, missing:Int = False)
		Self.value = value
		Self.kind = kind
		Self.missing = missing
	End Method
	
	Public
	Function Create:TLexerToken(value:String, kind:TTokenKind, missing:Int = False)
		Return New TLexerToken(value, kind, missing)
	End Function
	
	Method CodeInfo:SCodeInfo()
		If missing Then
			Return New SCodeInfo()
		Else
			' the assumptions made here are that:
			' - only these kinds of tokens can contain a newline
			' - they can only contain a single one
			' - it will be the last character of the value (the lexer turns ~r~n into ~n)
			' this must be kept in sync with the flex file
			If (kind = TTokenKind.Linebreak Or kind = TTokenKind.RemComment) And value And (value[value.length - 1] = Asc("~r") Or value[value.length - 1] = Asc("~n")) Then
				Assert Not ContainsLinebreaks(value[value.length - 1]) Else "Unexpected linebreak in token"
				Return New SCodeInfo(value.length, 1, 0)
			Else
				Assert Not ContainsLinebreaks(value) Else "Unexpected linebreak in token"
				Return New SCodeInfo(value.length, 0, value.length)
			End If
			
			Function ContainsLinebreaks:Int(str:String)
				For Local c:Int = 0 Until str.length
					If str[c] = Asc("~r") Or str[c] = Asc("~n") Then Return True
				Next
				Return False
			End Function
		End If
	End Method
	
	Method ToString:String() Override
		Function Escape:String(s:String)
			Return s.Replace("~r", "~~r").Replace("~n", "~~n")
		End Function
		Return kind.ToString() + " ~q" + Escape(value) + "~q"
	End Method
End Type
