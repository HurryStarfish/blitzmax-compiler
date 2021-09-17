SuperStrict
Framework BRL.Blitz
Import BRL.FileSystem
'Import BRL.TextStream
Import BRL.SystemDefault
Import BRL.StandardIO
Import "lexer/Lexer.bmx"
Import "parser/Parser.bmx"

Global testfilename:String = "test.bmx"
Global testfilepath:String = CurrentDir() + "/samples/" + testfilename



Rem
backwards compatibility breaking changes:
- .. precedence should be set lower than boolean and relational operators
- metadata on a line with multiple declarators should no longer apply to all of them
- writing a semicolon directly after a Then turns it into a multi-line-If

TODO: catch more errors during parsing rather than leaving them to semantic analysis?
example:
Type T
	Method M1()
		...
	Method M2()
		...
	End Method
End Type
if the parser forbids Methods inside Methods, this can be recognized as a forgotten End Method
before Method M2; if arbitrary callable declarations are accepted, it could also be a forgotten
End Method before the other End Method (M2 nested inside M1)
compare:
Type T
	Method M1()
		...
		Method M2()
			...
		End Method
	End Method
End Type
if the parser allows this, however, semantic analysis may be able to give a more useful error message
End Rem



Local ms1:Int = MilliSecs()

TestLexer 'False
TestParserTokens 'False
TestParser 'False

Local ms2:Int = MilliSecs()
Print "time: " + (ms2 - ms1) + "ms"


Function TestLexer(doPrint:Int = True)
	Local lexer:ILexer = New TLexer(testfilepath)
	Repeat
		Local token:TLexerToken = lexer.NextToken()
		If token = EOFToken Then Exit
		If doPrint Then Print token.ToString()
	Forever
End Function

Function TestParserTokens(doPrint:Int = True)
	Local parser:TParser = New TParser(New TLexer(testfilepath))
	Repeat
		Local token:TSyntaxToken = parser.currentToken
		If doPrint Then
			'Print token.ToString()
			Function TriviaToString:String(trivia:TLexerToken[])
				Local str:String
				For Local t:TLexerToken = EachIn trivia
					If str Then str :+ ", "
					str :+ Quote(t.value)
				Next
				Return "[" + str + "]"
			End Function
			Function Quote:String(str:String)
				Return "~q" + str.Replace("~t", "~~t").Replace("~n", "~~n") + "~q"
			End Function
			Local str:String
			str :+ TriviaToString(token.leadingTrivia)
			str :+ " <<< "
			str :+ token.lexerToken.kind.ToString() + " " + Quote(token.lexerToken.value)
			str :+ " >>> "
			str :+ TriviaToString(token.trailingTrivia)
			Print str
		End If
		parser.AdvanceToNextSyntaxToken
		If token.lexerToken = EOFToken Then Exit
	Forever
End Function

Function TestParser(doPrint:Int = True)
	Local parser:IParser = New TParser(New TLexer(testfilepath))
	Local x:ISyntax = parser.ParseCompilationUnit()
	If doPrint Then
		For Local error:TParseError = EachIn parser.Errors()
			Print "PARSE ERROR: " + error.ToString()
			Notify "PARSE ERROR: " + error.ToString()
		Next
		Print SyntaxToString(x)
		
		Print "-------------------------"
		
		Print SyntaxToCode(x)
	End If
End Function


