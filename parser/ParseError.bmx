SuperStrict
Import "../syntax/SyntaxToken.bmx"



Type TParseError
	Field ReadOnly token:TSyntaxToken
	Field ReadOnly message:String
	
	Method New(token:TSyntaxToken, message:String)
		Self.token = token
		Self.message = message
	End Method
	
	Method ToString:String() Override
		Return message + " at token " + token.lexerToken.ToString()
	End Method
End Type
