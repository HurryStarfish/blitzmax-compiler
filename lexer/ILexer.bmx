SuperStrict
Import "LexerToken.bmx"
Import "LexerExceptions.bmx"



Interface ILexer
	' constructor may throw TLexerFileReadException
	Method NextToken:TLexerToken()
	Method GetPosition:ILexerPosition()
	Method SetPosition(position:ILexerPosition) ' lexer only needs to accept its own implementation
	'Method Errors:TLexError()
End Interface



Interface ILexerPosition End Interface
