SuperStrict
Import "LexerToken.bmx"



Interface ILexer
	Method NextToken:TLexerToken()
	Method GetPosition:ILexerPosition()
	Method SetPosition(position:ILexerPosition) ' lexer only needs to accept its own implementation
	'Method Errors:TLexError()
End Interface



Interface ILexerPosition End Interface
