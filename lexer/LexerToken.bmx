SuperStrict
Import "TokenKind.bmx"
Import "CodeLocation.bmx"
Import "CodeRange.bmx"



Global EOFToken:TLexerToken = New TLexerToken(Null, TTokenKind.Eof, Null)

Type TLexerToken Final
	Field ReadOnly value:String
	Field ReadOnly kind:TTokenKind
	Field ReadOnly codeLocation:SCodeLocation
	Field ReadOnly missing:Int'Bool
	
	Private
	Method New() End Method
	
	Public
	Method New(value:String, kind:TTokenKind, codeLocation:SCodeLocation, missing:Int = False)
		Self.value = value
		Self.kind = kind
		Self.codeLocation = codeLocation
		Self.missing = missing
	End Method
	
	Method CodeRange:SCodeRange()
		If Not codeLocation.IsValid() Then Return New SCodeRange(codeLocation, codeLocation)
		
		Local startLocation:SCodeLocation = codeLocation
		Local endLocation:SCodeLocation
		If missing Then
			endLocation = startLocation
		Else If (kind = TTokenKind.Linebreak Or kind = TTokenKind.RemComment) And value And (value[value.length - 1] = Asc("\n") Or value[value.length - 1] = Asc("\r")) Then
			' the assumptions here are that only these token kinds can contain a newline,
			' that they can only contain a single one, and that it will be the last character of the value
			' this must be kept in sync with the flex file
			endLocation = New SCodeLocation(startLocation.filePath, startLocation.line + 1, 1)
		Else
			endLocation = New SCodeLocation(startLocation.filePath, startLocation.line, startLocation.column + value.length)
		End If
		Return New SCodeRange(startLocation, endLocation)
	End Method
	
	Method ToString:String() Override
		Function Escape:String(s:String)
			Return s.Replace("~r", "~~r").Replace("~n", "~~n")
		End Function
		Return kind.ToString() + " ~q" + Escape(value) + "~q     (" + codeLocation.ToString() + ")"
	End Method
End Type
