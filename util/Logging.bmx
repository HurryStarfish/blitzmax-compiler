SuperStrict
Import BRL.StandardIO



Global ActiveLogCategories:ELogCategory[4] ' TODO: enum array literals are broken
Local a:ELogCategory[4]
ActiveLogCategories[0] = ELogCategory.LexerTokens
ActiveLogCategories[1] = ELogCategory.ParserTokens
ActiveLogCategories[2] = ELogCategory.TypeCreation
ActiveLogCategories[3] = ELogCategory.ScopeLookup



Enum ELogCategory
	LexerTokens
	ParserTokens
	TypeCreation
	ScopeLookup
End Enum



Function LoggingActive:Int(category:ELogCategory)
	For Local c:ELogCategory = EachIn ActiveLogCategories
		If category = c Then Return True
	Next
	Return False
End Function

Function Log(category:ELogCategory, message:String)
	Print "[" + category.ToString() + "] " + message
End Function
