SuperStrict
Framework BRL.Blitz
Import BRL.FileSystem
'Import BRL.TextStream
Import BRL.SystemDefault
Import BRL.StandardIO
Import "lexer/Lexer.bmx"
Import "parser/Parser.bmx"
Import "syntax/SyntaxVisitor.bmx"
Import "syntax/SyntaxUtils.bmx"
Import "symbols/Visitors.bmx"

Global testfiledir:String = CurrentDir() + "/samples/"
Global testfilename:String = "declarations.bmx"
Global testfilepath:String = testfiledir + testfilename
StandardIOStream = New TMultiStreamWrapper([StandardIOStream, WriteFile(testfiledir + "output.txt")])



Rem
backwards compatibility breaking changes:
- .. precedence should be set higher than boolean and relational operators
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

'TestLexer 'False
'TestParserTokens 'False
'TestParser 'False
TestSymbols

Local ms2:Int = MilliSecs()
Print "time: " + (ms2 - ms1) + "ms"

Function TestLexer(doPrint:Int = True)
	Local lexer:ILexer = New TLexer(testfilepath)
	Repeat
		Local token:TLexerToken = lexer.NextToken()
		If token.kind = TTokenKind.Eof Then Exit
		If doPrint Then Print token.ToString()
	Forever
End Function

Function TestParserTokens(doPrint:Int = True)
	Local parser:TParser = New TParser(testfilepath, CreateLexer)
	Local x:TSyntaxTree = New TSyntaxTree(parser.ParseCompilationUnit())
	New TSyntaxTokenVisitor(doPrint).Visit x
End Function
Type TSyntaxTokenVisitor Extends TSyntaxVisitor
	Field doPrint:Int
	Method New(doPrint:Int) Self.doPrint = doPrint End Method
	Method VisitTopDown(token:TSyntaxToken)
		If doPrint Then
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
	End Method
End Type

Function TestParser(doPrint:Int = True)
	Local parser:IParser = New TParser(testfilepath, CreateLexer)
	Local x:TSyntaxTree = New TSyntaxTree(parser.ParseCompilationUnit())
	If doPrint Then
		For Local error:TParseError = EachIn parser.Errors()
			Print "PARSE ERROR: " + error.ToString()
		Next
		Print SyntaxToString(x.GetRootSyntax())
		
		Print "-------------------------"
		
		New TGenTestVisitor.Visit x
		
		Local cv:TSyntaxToCodeVisitor = New TSyntaxToCodeVisitor()
		cv.Visit x
		For Local r:TSyntaxToCodeVisitorFileResult = EachIn cv.fileResults
			Print "~~~~~~~~~~ " + r.codeFilePath + " ~~~~~~~~~~"
			Print r.stringBuilder.ToString()
		Next
	End If
End Function

Function TestSymbols(doPrint:Int = True)
	Local parser:IParser = New TParser(testfilepath, CreateLexer)
	Local x:TSyntaxTree = New TSyntaxTree(parser.ParseCompilationUnit())
	If doPrint Then
		For Local error:TParseError = EachIn parser.Errors()
			Print "PARSE ERROR: " + error.ToString()
		Next
	End If
	
	Local vs:TCreateScopesVisitor = New TCreateScopesVisitor
	vs.Visit x
	Local vt:TCollectTypeDeclarationSyntaxesVisitor = New TCollectTypeDeclarationSyntaxesVisitor
	vt.Visit x
	Local scopeTree:TScopeTree = New TScopeTree(vs.scopes)
	CreateTypeDeclarations TTypeDeclarationSyntax[](vt.typeDeclarationSyntaxes.ToArray()), scopeTree
		
	If doPrint Then
		For Local error:TSemanticError = EachIn SemanticErrors
			Print "COMPILE ERROR: " + error.ToString()
		Next
		'Local vs:TCreateScopesAndInsertDeclarationsVisitor = New TCreateScopesAndInsertDeclarationsVisitor
		'vs.Visit x
		'Print SyntaxToString(x.GetRootSyntax(), ESyntaxToStringOptions.ShowMinorFields | ESyntaxToStringOptions.ShowCodeRanges, vs.scopes)
		Print SyntaxToString(x.GetRootSyntax(), ESyntaxToStringOptions.ShowCodeRanges, vs.scopes)
		Print
	End If
End Function



Function CreateLexer:ILexer(filePath:String)
	Return New TLexer(filePath)
End Function



Type TGenTestVisitor Extends TSyntaxVisitor
	Field str:String = ""
	Method VisitTopDown(s:TStatementSeparatorSyntax)
		If str And Not str.EndsWith("<-") Then
			Print str
		End If
		str = ""
	End Method
	Method VisitTopDown(s:IStatementSyntax)
		Local sstr:String = SyntaxToCode(s)
		If sstr.Find("'") <> -1 Then sstr = sstr[..sstr.Find("'")]
		str :+ sstr + "   <-"
	End Method
	Method VisitTopDown(s:TRelationalExpressionSyntax)
		str :+ " rel"
	End Method
	Method VisitTopDown(s:TTypeApplicationExpressionSyntax)
		str :+ " gen"
	End Method
End Type





Type TMultiStreamWrapper Extends TStream Final ' TODO: exception handling
	Field ReadOnly streams:TStream[]
	
	Method New(streams:TStream[])
		Self.streams = streams
	End Method
	
	Method Flush() Override
		For Local s:TStream = EachIn streams
			s.Flush
		Next
	End Method
	
	Method Close() Override
		For Local s:TStream = EachIn streams
			s.Close
		Next
	End Method
	
	Method Read:Long(buf:Byte Ptr, count:Long) Override
		Throw "no"
	End Method
	
	Method Write:Long(buf:Byte Ptr, count:Long) Override
		Local writtenMin:Long = -1
		For Local s:TStream = EachIn streams
			Local written:Long = s.Write(buf, count)
			If writtenMin = -1 Or written < writtenMin Then writtenMin = written
		Next
		Return writtenMin
	End Method
	
	Method ReadByte:Int() Override
		Throw "no"
	End Method
	
	Method WriteByte(n:Int) Override
		For Local s:TStream = EachIn streams
			s.WriteByte n
		Next
	End Method
	
	Method ReadShort:Int() Override
		Throw "no"
	End Method
	
	Method WriteShort(n:Int) Override
		For Local s:TStream = EachIn streams
			s.WriteShort n
		Next
	End Method
	
	Method ReadInt:Int() Override
		Throw "no"
	End Method
	
	Method WriteInt(n:Int) Override
		For Local s:TStream = EachIn streams
			s.WriteInt n
		Next
	End Method
	
	Method ReadFloat:Float() Override
		Throw "no"
	End Method
	
	Method WriteFloat(n:Float) Override
		For Local s:TStream = EachIn streams
			s.WriteFloat n
		Next
	End Method
	
	Method ReadDouble:Double() Override
		Throw "no"
	End Method
	
	Method WriteDouble(n:Double) Override
		For Local s:TStream = EachIn streams
			s.WriteDouble n
		Next
	End Method
	
	Method ReadLine:String() Override
		Throw "no"
	End Method
	
	Method WriteLine:Int(t:String) Override
		For Local s:TStream = EachIn streams
			s.WriteLine t
		Next
	End Method
	
	Method ReadString:String(n:Int) Override
		Throw "no"
	End Method
	
	Method WriteString(t:String) Override
		For Local s:TStream = EachIn streams
			s.WriteString t
		Next
	End Method
	
	Method ReadObject:Object() Override
		Throw "no"
	End Method
	
	Method WriteObject(obj:Object) Override
		For Local s:TStream = EachIn streams
			s.WriteObject obj
		Next
	End Method
End Type
