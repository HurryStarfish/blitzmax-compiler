SuperStrict
Import "../syntax/SyntaxToken.bmx"



Type TParseError Final
	Field ReadOnly message:String
	Field ReadOnly token:TSyntaxToken
	
	Private
	Method New() End Method
	
	Public
	Method New(message:String, token:TSyntaxToken)
		Self.message = message
		Self.token = token
	End Method
	
	Method ToString:String() Override
		Return message + " (at token " + token.lexerToken.ToString() + ")"
	End Method
End Type
