SuperStrict
'Import "../syntax/SyntaxToken.bmx"



Type TLexError
	Field ReadOnly token:TToken
	Field ReadOnly message:String
	
	Method New(token:TToken, message:String)
		Self.token = token
		Self.message = message
	End Method
	
	Method ToString:String() Override
		Return message + " at token " + token.ToString()
	End Method
End Type
