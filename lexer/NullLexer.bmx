SuperStrict
Import "ILexer.bmx"



Type TNullLexer Implements ILexer Final
	Private
	Global eofToken:TLexerToken = TLexerToken.Create(Null, TTokenKind.Eof, Null)
	
	Public
	Method NextToken:TLexerToken() Override
		Return eofToken
	End Method
	
	Method GetPosition:ILexerPosition() Override
		Return New TNullLexerPosition
	End Method
	
	Method SetPosition(position:ILexerPosition) Override
	End Method
End Type



Private

Type TNullLexerPosition Implements ILexerPosition Final
End Type
