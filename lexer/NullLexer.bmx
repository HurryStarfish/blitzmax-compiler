SuperStrict
Import "ILexer.bmx"



Type TNullLexer Implements ILexer Final
	Method NextToken:TLexerToken() Override
		Return EOFToken
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
