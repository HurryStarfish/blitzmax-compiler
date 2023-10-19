SuperStrict
Import "SyntaxBase.bmx"
Import "../lexer/LexerToken.bmx"



Type TSyntaxToken Implements ISyntaxOrSyntaxToken, ISyntaxDataOrSyntaxToken Final
	Field ReadOnly lexerToken:TLexerToken
	Field ReadOnly leadingTrivia:TLexerToken[]
	Field ReadOnly trailingTrivia:TLexerToken[]
	
	Function Create:TSyntaxToken(lexerToken:TLexerToken, leadingTrivia:TLexerToken[], trailingTrivia:TLexerToken[])
		Return New TSyntaxToken(lexerToken, leadingTrivia, trailingTrivia)
	End Function
	
	Protected
	Method New(lexerToken:TLexerToken, leadingTrivia:TLexerToken[], trailingTrivia:TLexerToken[])
		Self.lexerToken = lexerToken
		Self.leadingTrivia = leadingTrivia
		Self.trailingTrivia = trailingTrivia
	End Method
	
	Public
	Method Kind:TTokenKind()
		Return lexerToken.kind
	End Method
	
	Method CodeRange:SCodeRange() Override
		Return lexerToken.CodeRange()
	End Method
	
	Method ToString:String() Override
		Local codeRange:String
		'codeRange = "   " + Self.CodeRange().ToString()
		Local trivia:String
		' TODO
		'If leadingTrivia Or trailingTrivia Then
		'	trivia = " ("
		'	If leadingTrivia Then trivia :+ TriviaToString(leadingTrivia) + " <<<"
		'	If leadingTrivia And trailingTrivia Then trivia :+ " "
		'	If trailingTrivia Then trivia :+ ">>> " + TriviaToString(trailingTrivia)
		'	trivia :+ ")"
		'End If
		Local missing:String
		If lexerToken.missing Then missing = " (missing)"
		Return "TSyntaxToken " + lexerToken.kind.ToString() + " " + Escape(lexerToken.value) + codeRange + trivia + missing
		
		Function TriviaToString:String(lexerToken:TLexerToken[])
			Local str:String
			For Local t:TLexerToken = EachIn lexerToken
				If str Then str :+ ", "
				str :+ Escape(t.value)
			Next
			Return "[" + str + "]"
		End Function
		
		Function Escape:String(s:String)
			Return "~q" + s.Replace("~q", "~~q").Replace("~n", "~~n").Replace("~t", "~~t") + "~q"
		End Function
	End Method
End Type
