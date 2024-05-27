SuperStrict
Import BRL.StandardIO



Enum ELogCategory
	LexerTokens
	ParserTokens
	TypeCreation
	ValueCreation
	TypeLookup
	ScopeLookup
End Enum

Global ActiveLogCategories:ELogCategory[] = [ ..
	ELogCategory.LexerTokens, ..
	ELogCategory.ParserTokens, ..
	ELogCategory.TypeCreation, ..
	ELogCategory.ValueCreation, ..
	ELogCategory.TypeLookup, ..
	ELogCategory.ScopeLookup ..
]



Function LoggingActive:Int(category:ELogCategory)
	For Local c:ELogCategory = EachIn ActiveLogCategories
		If category = c Then Return True
	Next
	Return False
End Function

Function Log(category:ELogCategory, message:String)
	Print "[" + category.ToString() + "] " + message
End Function
