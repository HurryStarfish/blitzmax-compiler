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
			str :+ "   " + token.CodeRange().ToString()
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
			'Notify "PARSE ERROR: " + error.ToString()
		Next
		Print SyntaxToString(x.GetRootSyntax())
		
		Print "-------------------------"
		
		'Print SyntaxToCode(x)
		''DebugStop
		''VisitSyntax New TTestVisitor, x
		'Print "~~~~~~~~~~~~"
		New TGenTestVisitor.Visit x
	End If
End Function

Function TestSymbols(doPrint:Int = True)
	Local parser:IParser = New TParser(testfilepath, CreateLexer)
	Local x:TSyntaxTree = New TSyntaxTree(parser.ParseCompilationUnit())
	If doPrint Then
		For Local error:TParseError = EachIn parser.Errors()
			Print "PARSE ERROR: " + error.ToString()
			'Notify "PARSE ERROR: " + error.ToString()
		Next
		
		Local vs:TCreateScopesVisitor = New TCreateScopesVisitor
		vs.Visit x
		Local vt:TCollectTypeDeclarationSyntaxesVisitor = New TCollectTypeDeclarationSyntaxesVisitor
		vt.Visit x
		
		Local scopeTree:TScopeTree = New TScopeTree
		scopeTree.scopes = vs.scopes
		CreateTypeDeclarations TTypeDeclarationSyntax[](vt.typeDeclarationSyntaxes.ToArray()), scopeTree
		
		For Local error:TSemanticError = EachIn SemanticErrors
			Print "COMPILE ERROR: " + error.ToString()
			'Notify "COMPILE ERROR: " + error.ToString()
		Next
		
		'Local vs:TCreateScopesAndInsertDeclarationsVisitor = New TCreateScopesAndInsertDeclarationsVisitor
		'vs.Visit x
		Print SyntaxToString(x.GetRootSyntax(), ESyntaxToStringOptions.ShowMinorFields | ESyntaxToStringOptions.ShowCodeRanges, vs.scopes)
	End If
End Function



Rem
Type TTypeResolutionVisitor Extends TSyntaxVisitor
	Field types:TMap = New TMap'<ISyntax, TType>
	
	Method VisitBottomUp(link:TSyntaxLink, s:TNumericLiteralExpressionSyntax)
		Local type_:TTypeSymbol
		Select s.value.kind
			Case TTokenKind.IntLiteral, TTokenKind.HexIntLiteral, TTokenKind.BinIntLiteral type_ = TType.Int_
			Case TTokenKind.FloatLiteral type_ = TType.Float_
			Default Throw "Missing case"
		End Select
		If s.type_ Then
			type_ = TypeSyntaxToType(s.type_)
		End If
		types.Insert s, type_
	End Method
	
	Method VisitBottomUp(link:TSyntaxLink, s:TStringLiteralExpressionSyntax)
		Local type_:TType = TType.String_
		If s.type_ Then
			type_ = TypeSyntaxToType(s.type_)
		End If
		types.Insert s, TType.String_
	End Method
	
	Method VisitBottomUp(link:TSyntaxLink, s:TSumExpressionSyntax)
		Local lhsType:String = String types[s.lhs]
		Local rhsType:String = String types[s.rhs]
		'types.Insert s, lhsType + "+" + rhsType
	End Method
	
	Function TypeSyntaxToType:TType(syntax:TTypeSyntax)
		Local type_:TType = TypeBaseSyntaxToType(syntax.base)
		' TODO: marshallingModifier
		' TODO: typeArguments
		For Local suffixSyntax:TTypeSuffixSyntax = EachIn syntax.suffixes
			type_ = TypeSuffixSyntaxToType(type_, suffixSyntax)
		Next
		Return type_

		Function TypeBaseSyntaxToType:TType(syntax:TTypeBaseSyntax) ' nullable param
			If Not syntax Then Return TType.Void
			If TQualifiedNameTypeBaseSyntax(syntax) Then Return QualifiedNameTypeBaseSyntaxToType(TQualifiedNameTypeBaseSyntax(syntax))
			If TKeywordTypeBaseSyntax(syntax) Then Return KeywordTypeBaseSyntaxToType(TKeywordTypeBaseSyntax(syntax))
			If TSigilTypeBaseSyntax(syntax) Then Return SigilTypeBaseSyntaxToType(TSigilTypeBaseSyntax(syntax))
			Throw "Missing case"
			
			Function QualifiedNameTypeBaseSyntaxToType:TType(syntax:TQualifiedNameTypeBaseSyntax)
				Throw "TODO"
			End Function
			
			Function KeywordTypeBaseSyntaxToType:TType(syntax:TKeywordTypeBaseSyntax)
				Select syntax.keyword.kind
					Case TTokenKind.Byte_      Return TType.Byte_
					Case TTokenKind.Short_     Return TType.Short_
					Case TTokenKind.Int_       Return TType.Int_
					Case TTokenKind.UInt_      Return TType.UInt_
					Case TTokenKind.Long_      Return TType.Long_
					Case TTokenKind.ULong_     Return TType.ULong_
					'Case TTokenKind.Size_T_    Return TType.
					Case TTokenKind.Float_     Return TType.Float_
					Case TTokenKind.Double_    Return TType.Double_
					Case TTokenKind.String_    Return TType.String_
					Case TTokenKind.Object_    Return TType.Object_
					'Case TTokenKind.LParam_    Return TType.
					'Case TTokenKind.WParam_    Return TType.
					'Case TTokenKind.Float64_   Return TType.
					'Case TTokenKind.Float128_  Return TType.
					'Case TTokenKind.Double128_ Return TType.
					'Case TTokenKind.Int128_    Return TType.
					Default Throw "Missing case"
				End Select
			End Function
			
			Function SigilTypeBaseSyntaxToType:TType(syntax:TSigilTypeBaseSyntax)
				Select syntax.sigil.kind
					Case TTokenKind.ByteSigil   Return TType.Byte_
					Case TTokenKind.IntSigil    Return TType.Int_
					Case TTokenKind.FloatSigil  Return TType.Float_
					Case TTokenKind.DoubleSigil Return TType.Double_
					Case TTokenKind.StringSigil Return TType.String_
					Default Throw "Missing case"
				End Select
			End Function
		End Function
		
		Function TypeSuffixSyntaxToType:TType(baseType:TType, syntax:TTypeSuffixSyntax)
			If TPtrTypeSuffixSyntax(syntax) Then Return PtrTypeSuffixSyntaxToType(baseType, TPtrTypeSuffixSyntax(syntax))
			If TVarTypeSuffixSyntax(syntax) Then Return VarTypeSuffixSyntaxToType(baseType, TVarTypeSuffixSyntax(syntax))
			If TArrayTypeSuffixSyntax(syntax) Then Return ArrayTypeSuffixSyntaxToType(baseType, TArrayTypeSuffixSyntax(syntax))
			If TCallableTypeSuffixSyntax(syntax) Then Return CallableTypeSuffixSyntaxToType(baseType, TCallableTypeSuffixSyntax(syntax))
			Throw "Missing case"
			
			Function PtrTypeSuffixSyntaxToType:TType(baseType:TType, syntax:TPtrTypeSuffixSyntax)
				Throw "TODO"'Return baseType.ToPtrType()
			End Function
			
			Function VarTypeSuffixSyntaxToType:TType(baseType:TType, syntax:TVarTypeSuffixSyntax)
				Throw "TODO"'Return baseType.ToVarType()
			End Function
			
			Function ArrayTypeSuffixSyntaxToType:TType(baseType:TType, syntax:TArrayTypeSuffixSyntax)
				Throw "TODO"
			End Function
			
			Function CallableTypeSuffixSyntaxToType:TType(baseType:TType, syntax:TCallableTypeSuffixSyntax)
				Local parameterTypes:TType[]
				Local parameterDeclaratorListElements:TVariableDeclaratorListElementSyntax[] = syntax.parameterDeclaration.declarators.elements
				For Local element:TVariableDeclaratorListElementSyntax = EachIn parameterDeclaratorListElements
					parameterTypes :+ [TypeSyntaxToType(element.declarator.type_)]
				Next
				Throw "TODO"'Return baseType.ToFunctionType(parameterTypes)
			End Function

		End Function
	End Function
End Type
End Rem
Type TPrintSemanticVisitor Extends TSyntaxVisitor
	Method VisitTopDown(s:ISyntax)
		'StandardIOStream.WriteString SyntaxToString(s)
	End Method
End Type



Function CreateLexer:ILexer(filePath:String)
	Return New TLexer(filePath)
End Function



Type TGenTestVisitor Extends TSyntaxVisitor
	Field str:String = ""
	Method VisitTopDown(s:TSyntax) ' combined instead of separate visitors because of reflection bug
		If TStatementSeparatorSyntax(s) Then
			If str And Not str.EndsWith("<-") Then
				Print str
			End If
			str = ""
		Else If IStatementSyntax(s) Then
			Local sstr:String = SyntaxToCode(s)
			If sstr.Find("'") <> -1 Then sstr = sstr[..sstr.Find("'")]
			str :+ sstr + "   <-"
		End If
	End Method
	Method VisitTopDown(s:TRelationalExpressionSyntax)
		str :+ " rel"
	End Method
	Method VisitTopDown(s:TTypeApplicationExpressionSyntax)
		str :+ " gen"
	End Method
End Type



Type TTestVisitor Extends TSyntaxVisitor
	Rem
	Method VisitTopDown(f:TForStatementSyntax)
		Print "Found For statement at:"
		Print f.CodeRange().ToString()
	End Method
	Method VisitTopDown(f:TExpressionListSyntax)
		Print "Found expression list at:"
		DebugStop
		Print f.CodeRange().ToString()
	End Method
	End Rem
	Method VisitTopDown(f:ISyntax)
		Handle f
	End Method
	
	'Method VisitTopDown(f:TSyntaxToken)
	'	Handle f
	'End Method

	Method Handle(f:ISyntaxOrSyntaxToken)
		'If Not IStatementSyntax(f) Then Return
		Print "Found " + TTypeId.ForObject(f).Name() + " at:"
		Print ISyntax(f).CodeRange().ToString()
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
