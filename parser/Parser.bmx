SuperStrict
Import "IParser.bmx"
Import "../lexer/ILexer.bmx"
Import "../lexer/NullLexer.bmx"
Import "../syntax/Syntax.bmx"



Private

Const ThrowOnParseError:Int = False ' for debugging

' for declaration parsing:

Enum ETypeKind
	Class
	Struct_
	Interface_
	Enum_
End Enum

Enum ECallableKind
	Function_
	Method_
End Enum

Enum EMultipleDeclaratorsOption
	Disallow
	Allow
End Enum

Enum EInitializersOption
	Disallow
	Allow
	Require
End Enum

' for type parsing:

Enum ETypeParseMode
	Committal
	Noncommittal
	' committal parsing means locking into success after having successfully parsed the first token
	' and is to be used in contexts where a (possibly optional) type is expected and the first token
	' is sufficient to determine whether that type is present;
	' noncommittal parsing means using backtracking to fail late if no complete type can be parsed
	' and is to be used in contexts where either a type or some other syntax is expected and the first
	' token is insufficient to distinguish between them
End Enum

Enum EColonTypeMode
	Colon ' omitted in void-returning callable types
	NoColon
End Enum

Enum ECallableTypeOption
	Disallow
	Allow
End Enum

Enum EArrayDimensionsOption
	Disallow
	Allow
	Require
End Enum

' for list parsing:

Enum EEmptyElementsOption
	Disallow
	Allow
	Require
End Enum

' for assignment parsing:

Enum EAssignmentMode
	Regular
	RegularOrCompound
	Constant
End Enum



Struct SParserState
	Private
	Field ReadOnly lexerPosition:ILexerPosition
	Field ReadOnly currentToken:TSyntaxToken
	Field ReadOnly nextLexerToken:TLexerToken
	Field ReadOnly nextLeadingTrivia:TLexerToken[]
	Field ReadOnly parseErrors:TList'<TParseError>
	Field ReadOnly terminatorStack:TTerminatorStack
	
	Public
	Method New(lexerPosition:ILexerPosition, currentToken:TSyntaxToken, nextLexerToken:TLexerToken, nextLeadingTrivia:TLexerToken[], parseErrors:TList, terminatorStack:TTerminatorStack)
		Self.lexerPosition = lexerPosition
		Self.currentToken = currentToken
		Self.nextLexerToken = nextLexerToken
		Self.nextLeadingTrivia = nextLeadingTrivia
		Self.parseErrors = parseErrors
		Self.terminatorStack = terminatorStack
	End Method
End Struct

Type TTerminatorStack Final
	Private
	Field stack:TTokenKind[][8]
	Field stackSize:Int = 0
	
	Public
	Method Copy:TTerminatorStack()
		Local c:TTerminatorStack = New TTerminatorStack
		c.stack = Self.stack[..Self.stackSize]
		c.stackSize = Self.stackSize
		Return c
	End Method
	
	Method Push(possibleTerminatorKinds:TTokenKind[]) ' multiple options are possible
		Assert possibleTerminatorKinds Else "Must specify at least one terminator"
		If stack.length = stackSize Then stack = stack[..stack.length * 2]
		stack[stackSize] = possibleTerminatorKinds
		stackSize :+ 1
	End Method
	
	Method Pop()
		Assert stackSize > 0 Else "Cannot pop terminator from empty stack"
		stack[stackSize - 1] = Null
		stackSize :- 1
	End Method
	
	Method Contains:Int(terminatorKind:TTokenKind)
		For Local k:Int = stackSize Until 0 Step -1
			Local kinds:TTokenKind[] = stack[k - 1]
			For Local kind:TTokenKind = EachIn kinds
				If kind = terminatorKind Then Return True
			Next
		Next
		Return False
	End Method
End Type



Public

Type TParser Implements IParser Final
	
	Field currentToken:TSyntaxToken
	
	Private
	Global StatementSeparatorTokenKinds:TTokenKind[] = [TTokenKind.Linebreak, TTokenKind.Semicolon]
	
	Field ReadOnly filePath:String
	Field ReadOnly createLexer:ILexer(filePath:String)
	Field ReadOnly lexer:ILexer
	Field ReadOnly parentFileParser:TParser
	
	Field nextLexerToken:TLexerToken
	Field nextLeadingTrivia:TLexerToken[]
	
	Field parseErrors:TList = New TList'<TParseError>
	Field terminatorStack:TTerminatorStack = New TTerminatorStack
	
	Method New() End Method
	
	Method New(filePath:String, createLexer:ILexer(filePath:String), parentFileParser:TParser)
		Self.filePath = RealPath(filePath)
		Self.createLexer = createLexer
		Self.parentFileParser = parentFileParser
		
		Try
			Self.lexer = createLexer(Self.filePath)
			AdvanceToNextSyntaxToken()
		Catch ex:TLexerFileReadException
			Self.lexer = New TNullLexer
			AdvanceToNextSyntaxToken()
			ReportError ex.message
		End Try
	End Method
	
	Public
	Method New(filePath:String, createLexer:ILexer(filePath:String))
		New(filePath, createLexer, Null)
	End Method
	
	Method Errors:TList() Override '<TParseError>
		Return parseErrors
	End Method
	
	Method AdvanceToNextSyntaxToken() ' will also skip line continuations (line breaks escaped via ..)
		' - scan from nextLexerToken to the next non-trivia token after it
		' - nextLexerToken then becomes the new currentToken and the afromentioned non-trivia becomes the new nextLexerToken
		' - if the new currentToken is not a line break, any trivia encountered during this scan
		'   becomes its trailing trivia;
		'   if the new currentToken is a line break, the trivia becomes the leading trivia of
		'   the new nextLexerToken (currentToken after the next call) instead
		' - this means that only the first non-trivia token on a line can have leading trivia,
		'   that a line break can never have trailing trivia, and that a line break can have
		'   leading trivia only if it is the only non-trivia token on the line it is on
		'
		' - spaces, tabs and comments are always trivia
		' - line breaks and .. are normally non-trivia, but are considered trivia if they form a
		'   line continuation, that is, a .. followed by any amount of trivia followed by a line break
		' - semicolons are always non-trivia
		
		Function ScanToPotentialNonTriviaToken:TLexerToken(self_:TParser, t:TLexerToken Var, skippedTrivia:TLexerToken[] Var)
			skippedTrivia = []
			While IsDefiniteTrivia(t)
				skippedTrivia :+ [t]
				t = self_.lexer.NextToken()
			Wend
		End Function
		
		Local atLineStart:Int = Not currentToken Or currentToken.Kind() = TTokenKind.Linebreak
		If Not currentToken Then ' first call, start of file
			' go to the first non-trivia token and gather leading trivia on the way, which would normally have been done on the previous call
			' TODO: handle line continuations at the start of the file corrently
			nextLexerToken = lexer.NextToken()
			ScanToPotentialNonTriviaToken Self, nextLexerToken, nextLeadingTrivia
		End If
		
		Local t:TLexerToken = nextLexerToken ' scanning cursor
		
		Local newCurrentLexerToken:TLexerToken
		Local newCurrentLeadingTrivia:TLexerToken[]
		Local newCurrentTrailingTrivia:TLexerToken[]
		
		' set the new current token and leading trivia
		newCurrentLexerToken = t
		newCurrentLeadingTrivia = nextLeadingTrivia
		t = lexer.NextToken()
		
		' look ahead to gather trailing trivia
		Local triviaAfterNewCurrentToken:TLexerToken[]
		ScanToPotentialNonTriviaToken Self, t, triviaAfterNewCurrentToken
		
		' if a .. token is encountered, scanning to the next potential non-trivia is required
		' to check whether they are part of a line continuation (and thus trailing trivia) or not
		While t.kind = TTokenKind.DotDot
			Local dotDotPosition:ILexerPosition = lexer.GetPosition()
			Local dotDotToken:TLexerToken = t
			Local triviaAfterdotDotToken:TLexerToken[]
			t = lexer.NextToken()
			ScanToPotentialNonTriviaToken Self, t, triviaAfterdotDotToken
			If t.kind = TTokenKind.Linebreak Then
				' line continuation => treat as trivia and continue scanning
				triviaAfterNewCurrentToken :+ [dotDotToken] + triviaAfterdotDotToken + [t]
				Local triviaAfterLineBreakToken:TLexerToken[]
				t = lexer.NextToken()
				ScanToPotentialNonTriviaToken Self, t, triviaAfterLineBreakToken
				triviaAfterNewCurrentToken :+ triviaAfterLineBreakToken
			Else
				' not a line continuation => treat as non-trivia
				lexer.SetPosition dotDotPosition
				t = dotDotToken
				Exit
			End If
		Wend
		
		' if the new current token is a line break, the gathered trivia will become the leading trivia of the next one
		' otherwise, it will become the trailing trivia of this one
		If newCurrentLexerToken.kind = TTokenKind.Linebreak Then
			newCurrentTrailingTrivia = []
			nextLeadingTrivia = triviaAfterNewCurrentToken
		Else
			newCurrentTrailingTrivia = triviaAfterNewCurrentToken
			nextLeadingTrivia = []
		End If
		nextLexerToken = t
		currentToken = TSyntaxToken.Create(newCurrentLexerToken, newCurrentLeadingTrivia, newCurrentTrailingTrivia)
	End Method
	
	Private
	Function IsDefiniteTrivia:Int(token:TLexerToken)
		Return ..
			token.kind = TTokenKind.Whitespace Or ..
			token.kind = TTokenKind.Comment Or ..
			token.kind = TTokenKind.Rem_ Or ..
			token.kind = TTokenKind.RemComment Or ..
			token.kind = TTokenKind.EndRem_
	End Function
	
	Method SaveState:SParserState()
		Return New SParserState(lexer.GetPosition(), currentToken, nextLexerToken, nextLeadingTrivia, parseErrors.Copy(), terminatorStack.Copy())
	End Method
	
	Method RestoreState(pos:SParserState)
		lexer.SetPosition pos.lexerPosition
		currentToken = pos.currentToken
		nextLexerToken = pos.nextLexerToken
		nextLeadingTrivia = pos.nextLeadingTrivia
		parseErrors = pos.parseErrors
		terminatorStack = pos.terminatorStack
	End Method
	
	Public
	Method TakeToken:TSyntaxToken()
		' takes the current token
		Local token:TSyntaxToken = currentToken
		AdvanceToNextSyntaxToken()
		Return token
	End Method
	
	Method TakeToken:TSyntaxToken(kind:TTokenKind)
		' takes a token of the specified kind, reporting an error and generating a missing token if the current token does not match
		Local token:TSyntaxToken = TryTakeToken(kind)
		If token Then
			Return token
		Else
			ReportError "Expected " + kind.ToCode()
			Return GenerateMissingToken(kind)
		End If
	End Method
	
	Method TryTakeToken:TSyntaxToken(kind:TTokenKind)
		' tries takes a token of the specified kind, returning null if the current token does not match
		Local token:TSyntaxToken = currentToken
		If token.Kind() = kind Then
			AdvanceToNextSyntaxToken()
			Return token
		Else
			Return Null
		End If
	End Method
	
	Method TryTakeToken:TSyntaxToken(kinds:TTokenKind[])
		' tries takes a token of any of the specified kinds, returning null if the current token does not match
		For Local kind:TTokenKind = EachIn kinds
			Local token:TSyntaxToken = TryTakeToken(kind)
			If token Then Return token
		Next
	End Method
	
	Method SkipAndTakeToken:TSyntaxToken(kinds:TTokenKind[], generatedKindIfMissing:TTokenKind, additionalTerminatorKinds:TTokenKind[])
		' TODO: limit lookahead
		' takes a token of any of the specified kinds, either skipping all tokens before its next occurrence (if it is before any of the surrounding or specified terminator kinds), or generating a missing token and reporting an error
		Local token:TSyntaxToken = TryTakeToken(kinds)
		If token Then
			Return token
		Else
			' skip tokens until we either encounter one we want or a terminator
			' if it was a terminator, discard the skipped tokens and restore the original state
			terminatorStack.Push additionalTerminatorKinds
			Local skippedTokens:TSyntaxToken[]
			Local mustRestoreState:Int = False
			Local state:SParserState
			If Not terminatorStack.Contains(currentToken.Kind()) Then
				state = SaveState()
				Repeat
					skippedTokens :+ [TakeToken()]
					If terminatorStack.Contains(currentToken.Kind()) Then
						mustRestoreState = True
						Exit
					Else
						token = TryTakeToken(kinds)
						If token Then Exit
					End If
				Forever
			End If
			terminatorStack.Pop
			If mustRestoreState Then RestoreState state
			
			If token And skippedTokens Then
				' TODO: introduce a separate kind of trivia for skipped tokens instead of flattening them like this
				Local leadingTrivia:TLexerToken[]
				For Local st:TSyntaxToken = EachIn skippedTokens
					ReportError "Unexpected " + st.lexerToken.value
					leadingTrivia :+ st.leadingTrivia + [st.lexerToken] + st.trailingTrivia
				Next
				leadingTrivia :+ token.leadingTrivia
				token = TSyntaxToken.Create(token.lexerToken, leadingTrivia, token.trailingTrivia)
			Else If Not token Then
				token = GenerateMissingToken(generatedKindIfMissing)
				Local expectedKindsStr:String
				For Local k:Int = 0 Until kinds.length
					If k = 0 Then
						expectedKindsStr :+ kinds[k].ToCode()
					Else If k = kinds.length - 1 Then
						expectedKindsStr :+ " or " + kinds[k].ToCode()
					Else
						expectedKindsStr :+ ", " + kinds[k].ToCode()
					End If
				Next
				ReportError "Expected " + expectedKindsStr
			End If
			Return token
		End If
	End Method
	
	Method SkipAndTakeToken:TSyntaxToken(kind:TTokenKind, additionalTerminatorKinds:TTokenKind[])
		' takes a token of the specified kind, either by skipping all tokens up to its next occurrence
		' (if one is found before any of the specified or surrounding terminator kinds), or,
		' if that fails, by generating a missing token and reporting an error
		Local token:TSyntaxToken = TryTakeToken(kind)
		
		If Not token Then
			Throw "TODO: sometimes drops tokens; fix" ' e.g. when skipping to the ">" in "a<b & c>(d)"
			terminatorStack.Push additionalTerminatorKinds
			Local skippedTokens:TSyntaxToken[]
			Local mustRestoreState:Int = False
			Local state:SParserState
			If Not terminatorStack.Contains(currentToken.Kind()) Then
				state = SaveState()
				Repeat
					skippedTokens :+ [TakeToken()]
					If currentToken.Kind() = kind Then
						token = TakeToken(kind)
						Exit
					Else If terminatorStack.Contains(currentToken.Kind()) Then
						mustRestoreState = True
						Exit
					Else If skippedTokens.length > 30 Then ' TODO
						mustRestoreState = True
						Exit
					End If
					' TODO: use additional terminators; in expression contexts this can be any token
					'       kind that can not appear in any expression (keywords for declarations,
					'       statements, ...), in other contexts it should not include any token
					'       kinds that could appear in the current construct
				Forever
			End If
			terminatorStack.Pop
			If mustRestoreState Then RestoreState state
			
			If token And skippedTokens Then
				' TODO: introduce a separate kind of trivia for skipped tokens instead of flattening them like this
				Local leadingTrivia:TLexerToken[]
				For Local st:TSyntaxToken = EachIn skippedTokens
					ReportError "Unexpected " + st.lexerToken.value
					leadingTrivia :+ st.leadingTrivia + [st.lexerToken] + st.trailingTrivia
				Next
				leadingTrivia :+ token.leadingTrivia
				token = TSyntaxToken.Create(token.lexerToken, leadingTrivia, token.trailingTrivia)
			Else If Not token Then
				token = TakeToken(kind)
			End If
		End If
		
		Return token
	End Method
	
	Method GenerateMissingToken:TSyntaxToken(kind:TTokenKind)
		Return GenerateMissingToken(kind, kind.ToCode())
	End Method
	
	Method GenerateMissingToken:TSyntaxToken(kind:TTokenKind, value:String)
		Local missingToken:TSyntaxToken = TSyntaxToken.Create(New TLexerToken(value, kind, True), [], [])
		Return missingToken
	End Method
	
	Method GenerateMissingIdentifier:TSyntaxToken()
		Return GenerateMissingToken(TTokenKind.Identifier, "<missing identifier>")
	End Method
	
	Method GenerateMissingName:TNameSyntaxData()
		Return TNameSyntaxData.Create(GenerateMissingIdentifier())
	End Method
	
	Method GenerateMissingQualifiedName:TQualifiedNameSyntaxData()
		Return TQualifiedNameSyntaxData.Create([TQualifiedNamePartSyntaxData.Create(Null, GenerateMissingIdentifier())])
	End Method
	
	Method GenerateMissingExpression:TNameExpressionSyntaxData() ' TODO: TErrorExpression?
		Return TNameExpressionSyntaxData.Create(GenerateMissingName())
	End Method
	
	Method GenerateMissingType:TTypeSyntaxData()
		Return TTypeSyntaxData.Create(Null, TQualifiedNameTypeBaseSyntaxData.Create(GenerateMissingQualifiedName()), Null, Null, [])
	End Method
	
	Method ReportError(message:String)
		' TODO: allow specifying the code location of the error
		parseErrors.AddLast New TParseError(message, currentToken)
		If ThrowOnParseError Then Throw message
	End Method
	
	' all Parse* methods either succeed and return a valid object (although they may still
	' report errors) or they fail and return Null (in which case they must restore the
	' parser to the same state it was in upon entering to avoid any tokens getting lost)
	' most Parse* methods immediately either fail or lock themselves into success after examining
	' the first token at the current position (making failure cheap), but some involve backtracking
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Top-Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseCompilationUnit:TCompilationUnitSyntaxData() Override ' always succeeds
		Local header:TCodeHeaderSyntaxData = ParseCodeHeader()
		Local body:TCodeBodySyntaxData = ParseCodeBody()
		Local eofToken:TSyntaxToken = TakeToken(TTokenKind.Eof)
		Return TCompilationUnitSyntaxData.Create(header, body, eofToken, Self.filePath)
	End Method
	
	Method ParseCodeHeader:TCodeHeaderSyntaxData() ' always succeeds
		Local elements:ICodeHeaderElementSyntaxData[]
		Repeat
			Local element:ICodeHeaderElementSyntaxData = ParseCodeHeaderElement()
			If Not element Then Exit
			elements :+ [element]
		Forever
		Return TCodeHeaderSyntaxData.Create(elements)
	End Method
	
	Method ParseCodeBody:TCodeBodySyntaxData() ' always succeeds
		Local block:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Eof])
		Return TCodeBodySyntaxData.Create(block)
	End Method
	
	Method ParseCodeBlock:TCodeBlockSyntaxData(expectedTerminatorKinds:TTokenKind[]) ' always succeeds
		Rem
		' this implements some basic "panic mode" error recovery by skipping tokens
		' the idea is to maintain a stack of scopes (or more specifically, their associated
		' terminator token kinds) and use it for error recovery
		' if parsing a code block element fails and the current token is one of the terminators
		' to be expected for the current block, then all is well; but if it is some other kind of
		' token then there are two possible options:
		' a) consider the code block completed and insert the missing terminator (outside of this method)
		' before proceeding to the unexpected token
		' b) skip the unexpected token and continue trying to parse elements for the code block
		' if we always choose option a), then an unexpected token (assuming it isn't recognized as the
		' first token of a block element) would lead to exiting every open block one after another
		' (until the erroneous token is caught by some non-codeblock parsing method)
		' if we always choose option b), then we would never exit the block (except if we happen to find
		' the expected terminator somewhere, belonging to another block)
		' neither are good options on their own, but if we look up the unexpected token in the stack
		' of expected terminators, then we can choose option a) if we find it and option b) otherwise
		' this gives us a good chance to get the parser back on track, although code block elements
		' after the unexpected token may end up in the wrong block (trying to prevent that from
		' happening would require analyzing indentation)
		' TODO: try to extract this functionality into a separate method, because...
		' TODO: apart from ParseCodeBlock, other parse methods that deal with different kinds of blocks
		'       (type declarations, Extern, Try, Select) should do the same
		' TODO: on a sub-expression scale, this may also be useful to deal with other tokens that
		'       can be paired up in an "initiator-terminator"-like pattern, such as parentheses,
		'       brackets, braces, For and =, = and To, If and Then (maybe), Assert and Else...
		
		' example code:
		If x Then
			For y = 1 To 2
				While z
					a
					b
				Forever
				c
			Next
			d
		End If
		
		' if a) is always chosen, it will be parsed as:
		If x Then
			For y = 1 To 2
				While z
					a
					b
				Wend ' <--- (missing)
			Next ' <--- (missing)
		End If ' <--- (missing)
		' we have now left all the blocks, so we were unable to recover
				Forever
				c
			Next
			d
		End If
		
		' if b) is always chosen, it will be parsed as:
		If x Then
			For y = 1 To 2
				While z
					a
					b
				'Forever ' <--- (skipped)
				c
			'Next ' <--- (skipped)
			d
		'End If ' <--- (skipped)
		' we have not left any block, so we were unable to recover
		
		' if a) or b) is chosen depending on the stack or expected terminators, it will be parsed as:
		If x Then
			For y = 1 To 2
				While z
					a
					b
				'Forever ' <--- (skipped, because Forever is not on the stack)
				c
				Wend ' <--- (missing, because Next is on the stack)
				' we have parsed c as part of the wrong scope, but have now recovered
			Next
			d
		End If
		End Rem
		
		terminatorStack.Push expectedTerminatorKinds
		
		Local skippedTokens:TSyntaxToken[]
		
		Local elements:ICodeBlockElementSyntaxData[]
		Repeat
			Local element:ICodeBlockElementSyntaxData = ParseCodeBlockElement()
			If element Then
				If skippedTokens Then
					elements :+ [TErrorSyntaxData.Create(skippedTokens)]
					skippedTokens = []
				End If
				elements :+ [element]
			Else
				If terminatorStack.Contains(currentToken.Kind()) Then
					Exit
				Else
					ReportError "Unexpected " + currentToken.lexerToken.value
					skippedTokens :+ [currentToken]
					AdvanceToNextSyntaxToken
				End If
			End If
		Forever
		
		terminatorStack.Pop
		
		Return TCodeBlockSyntaxData.Create(elements)
	End Method
	
	Method ParseCodeHeaderElement:ICodeHeaderElementSyntaxData()
		Local separator:TStatementSeparatorSyntaxData = ParseStatementSeparator()
		If separator Then
			Return separator
		Else
			Return ParseHeaderDirective()
		End If
	End Method
	
	Method ParseCodeBlockElement:ICodeBlockElementSyntaxData()
		Local element:ICodeBlockElementSyntaxData
		element = ParseStatementSeparator();     If element Then Return element
		element = ParseIncludeDirective();       If element Then Return element
		element = ParseExternBlock();            If element Then Return element
		element = ParseVisibilityDirective();    If element Then Return element
		element = ParseDeclarationOrStatement(); If element Then Return element
		Return Null
	End Method
	
	Method ParseDeclarationOrStatement:ICodeBlockElementSyntaxData()
		Local declarationOrStatement:ICodeBlockElementSyntaxData
		declarationOrStatement = ParseDeclaration(); If declarationOrStatement Then Return declarationOrStatement
		declarationOrStatement = ParseStatement();   If declarationOrStatement Then Return declarationOrStatement
		Return Null
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Header Directives ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseHeaderDirective:IHeaderDirectiveSyntaxData()
		Local headerDirective:IHeaderDirectiveSyntaxData
		headerDirective = ParseStrictnessDirective(); If headerDirective Then Return headerDirective
		headerDirective = ParseModuleDirective();     If headerDirective Then Return headerDirective
		headerDirective = ParseModuleInfoDirective(); If headerDirective Then Return headerDirective
		headerDirective = ParseFrameworkDirective();  If headerDirective Then Return headerDirective
		headerDirective = ParseImportDirective();     If headerDirective Then Return headerDirective
		Return Null
	End Method
	
	Method ParseStrictnessDirective:TStrictnessDirectiveSyntaxData()
		Local strictness:TSyntaxToken = TryTakeToken([TTokenKind.Strict_, TTokenKind.SuperStrict_])
		If Not strictness Then Return Null
		
		Return TStrictnessDirectiveSyntaxData.Create(strictness)
	End Method
	
	Method ParseModuleDirective:TModuleDirectiveSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Module_)
		If Not keyword Then Return Null
		
		Local moduleName:TQualifiedNameSyntaxData = ParseModuleName()
		If Not moduleName Then
			ReportError "Expected module name"
			moduleName = GenerateMissingQualifiedName()
		End If
		
		Return TModuleDirectiveSyntaxData.Create(keyword, moduleName)
	End Method
	
	Method ParseModuleInfoDirective:TModuleInfoDirectiveSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.ModuleInfo_)
		If Not keyword Then Return Null
		
		Local info:TStringLiteralExpressionSyntaxData = ParseStringLiteralExpression()
		If Not info Then
			ReportError "Expected module info"
			info = TStringLiteralExpressionSyntaxData.Create(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
		End If
		
		Return TModuleInfoDirectiveSyntaxData.Create(keyword, info)
	End Method
	
	Method ParseFrameworkDirective:TFrameworkDirectiveSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Framework_)
		If Not keyword Then Return Null
		
		Local moduleName:TQualifiedNameSyntaxData = ParseModuleName()
		If Not moduleName Then
			ReportError "Expected module name"
			moduleName = GenerateMissingQualifiedName()
		End If
		
		Return TFrameworkDirectiveSyntaxData.Create(keyword, moduleName)
	End Method
	
	Method ParseImportDirective:TImportDirectiveSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Import_)
		If Not keyword Then Return Null
		
		Local filePath:TStringLiteralExpressionSyntaxData = ParseStringLiteralExpression()
		If filePath Then Return TImportDirectiveSyntaxData.Create(keyword, TImportSourceSyntaxData.Create(Null, filePath))
		
		Local moduleName:TQualifiedNameSyntaxData = ParseModuleName()
		If moduleName Then Return TImportDirectiveSyntaxData.Create(keyword, TImportSourceSyntaxData.Create(moduleName, Null))
		
		' TODO: Private Import, Import ... As ...
		
		ReportError "Expected file path or module name"
		Return TImportDirectiveSyntaxData.Create(keyword, TImportSourceSyntaxData.Create(GenerateMissingQualifiedName(), Null))
	End Method
	
	Method ParseModuleName:TQualifiedNameSyntaxData()
		Return ParseQualifiedName()
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Include Directive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseIncludeDirective:TIncludeDirectiveSyntaxData()
		' Include directives could be expanded during lexing, parsing, or semantic analysis,
		' each with different advantages and disadvantages
		'       the advantage of lexing is not having to worry about header vs. body content
		'       and may thus allow making imports more flexible (usable anywhere?),
		'       the advantage of parsing is not having to build multi-file handling into the lexer
		'       and being able to have the expanded code in the syntax tree
		'       the advantage of semantic analysis is keeping both the lexer and the parser simpler
		'       and being able to represent include directives as simple nodes in the syntax tree
		' lexing:
		'   - no need to distinguish between header and body content
		'   - can be usable almost anywhere, independent of syntactic structures
		'     e.g.: Include "type header.bmx"; Field f:Int; Include "type terminator.bmx"
		'   - new tokens have to be created for every instance of Include, no reuse possible
		'   - additional lexer complexity for keeping track of token locations (for errors, refactoring)
		' parsing:
		'   - can be made usable in most places as long as it conforms to certain syntactic constraints
		'     e.g.: Type T; Include "type members.bmx"; End Type
		'   - distinguishable from normal code in the syntax tree (easy refactoring and error reporting)
		'   - additional parser complexity
		'     - requires some special handling for the node to be legal in different context
		'     - requires different parsing "entry points" depending on context
		' semantic analysis:
		'   - can be a simple node with no expanded code as children
		'   - very limited in where the Include can appear
		'   - complicates symbol resolution in the including file and parsing of the included file
		
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Include_)
		If Not keyword Then Return Null
		
		Local filePath:TStringLiteralExpressionSyntaxData = ParseStringLiteralExpression()
		If Not filePath Then
			ReportError "Expected file path"
			filePath = TStringLiteralExpressionSyntaxData.Create(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
		End If
		Local filePathStr:String = filePath.value.lexerToken.value
		Assert filePathStr.StartsWith("~q") And filePathStr.EndsWith("~q") Else "Quotes missing from string literal"
		filePathStr = filePathStr[1..filePathStr.length - 1]

		Function CombinePaths:String(includingFilePath:String, includedFilePath:String) ' TODO: get a library for this (cpath?)
			If IsAbsolutePath(includedFilePath) Then
				Return includedFilePath
			Else
				Return StripSlash(includingFilePath[..includingFilePath.length - StripDir(includingFilePath).length]) + "/" + includedFilePath
			End If
			Function IsAbsolutePath:Int(path:String)
				? Win32
					Return path.length >= 3 And (path[0] >= Asc("A") And path[0] <= Asc("Z") Or path[0] >= Asc("a") And path[0] <= Asc("z")) And path[1] = Asc(":") And (path[2] = "/" Or path[2] = "\") Or path.StartsWith("\")
				? MacOS Or OSX Or iOS Or Linux Or Android Or RaspberryPi
					Return path.StartsWith("/")
				? Not (Win32 Or MacOS Or OSX Or iOS Or Linux Or Android Or RaspberryPi)
					Not implemented
				?
			End Function
		End Function
		
		Local actualIncludeFilePath:String = RealPath(CombinePaths(Self.filePath, filePathStr))
		
		Local includedCode:TIncludedCodeSyntaxData = ParseIncludedCode(actualIncludeFilePath)
		
		Return TIncludeDirectiveSyntaxData.Create(keyword, filePath, includedCode)
	End Method
	
	Method ParseIncludedCode:TIncludedCodeSyntaxData(actualIncludeFilePath:String)
		Function HasParentWithFilePath:Int(parser:TParser, filePath:String)
			Return Has(parser.parentFileParser, filePath)
			Function Has:Int(parser:TParser, filePath:String)
				If Not parser Then Return False Else If parser.filePath = filePath Then Return True Else Return Has(parser.parentFileParser, filePath)
			End Function
		End Function
		If HasParentWithFilePath(Self, actualIncludeFilePath) Then
			ReportError "Cyclic include of file ~q" + StripDir(actualIncludeFilePath) + "~q"
			
			Local body:TCodeBodySyntaxData = TCodeBodySyntaxData.Create(TCodeBlockSyntaxData.Create([]))
			Local eofToken:TSyntaxToken = GenerateMissingToken(TTokenKind.Eof)
			
			Return TIncludedCodeSyntaxData.Create(body, eofToken, actualIncludeFilePath)
		Else
			Local parser:TParser = New TParser(actualIncludeFilePath, Self.createLexer, Self)
			Local body:TCodeBodySyntaxData = parser.ParseCodeBody()
			Local eofToken:TSyntaxToken = parser.TakeToken(TTokenKind.Eof)
			For Local error:TParseError = EachIn parser.parseErrors
				parseErrors.AddLast error
			Next
			
			Return TIncludedCodeSyntaxData.Create(body, eofToken, actualIncludeFilePath)
		End If
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseExternBlock:TExternBlockSyntaxData()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Extern_)
		If Not initiatorKeyword Then Return Null
		
		Local callingConvention:TSyntaxToken = TryTakeToken(TTokenKind.StringLiteral)
		
		Local elements:IExternBlockElementSyntaxData[]
		Repeat
			elements :+ ParseStatementSeparators()
			Local element:IExternBlockElementSyntaxData = ParseExternBlockElement()
			If Not element Then Exit
			elements :+ [element]
			' TODO: skip tokens until it is possible to parse an element or End Extern - or possibly the terminator token of a surrounding block?
		Forever
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(TTokenKind.EndExtern_)
		Return TExternBlockSyntaxData.Create(initiatorKeyword, callingConvention, elements, terminatorKeyword)
	End Method
	
	Method ParseExternBlockElement:IExternBlockElementSyntaxData()
		Local externBlockElement:IExternBlockElementSyntaxData
		'externBlockElement = ParseExternTypeDeclaration();     If externBlockElement Then Return externBlockElement
		externBlockElement = ParseExternFunctionDeclaration(); If externBlockElement Then Return externBlockElement
		'externBlockElement = ParseExternVariableDeclaration(); If externBlockElement Then Return externBlockElement
		Return Null
	End Method
	
	Method ParseDeclaration:IDeclarationSyntaxData()
		Local declaration:IDeclarationSyntaxData
		
		declaration = ParseTypeDeclaration();              If declaration Then Return declaration
		declaration = ParseCallableDeclaration();          If declaration Then Return declaration
		declaration = ParseCodeBlockVariableDeclaration(); If declaration Then Return declaration
		declaration = ParseTypeAliasDeclaration();         If declaration Then Return declaration
		
		declaration = ParseLabelDeclaration();             If declaration Then Return declaration
		
		Return Null
	End Method
	
	Method ParseTypeDeclaration:TTypeDeclarationSyntaxData()
		Local typeDeclaration:TTypeDeclarationSyntaxData
		typeDeclaration = ParseClassOrStructOrInterfaceDeclaration(); If typeDeclaration Then Return typeDeclaration
		typeDeclaration = ParseEnumDeclaration();                     If typeDeclaration Then Return typeDeclaration
		Return Null
	End Method
	
	Method ParseClassOrStructOrInterfaceDeclaration:TTypeDeclarationSyntaxData()
		Local kind:ETypeKind
		Local declarationTokenKind:TTokenKind
		Local terminatorTokenKind:TTokenKind
		Select currentToken.Kind()
			Case TTokenKind.Type_      kind = ETypeKind.Class;      declarationTokenKind = TTokenKind.Type_;      terminatorTokenKind = TTokenKind.EndType_
			Case TTokenKind.Struct_    kind = ETypeKind.Struct_;    declarationTokenKind = TTokenKind.Struct_;    terminatorTokenKind = TTokenKind.EndStruct_
			Case TTokenKind.Interface_ kind = ETypeKind.Interface_; declarationTokenKind = TTokenKind.Interface_; terminatorTokenKind = TTokenKind.EndInterface_
			Default Return Null
		End Select
		
		Local initiatorKeyword:TSyntaxToken = TakeToken(declarationTokenKind)
		
		Local name:TNameSyntaxData = ParseName()
		If Not name Then
			ReportError "Expected name"
			name = GenerateMissingName()
		End If
		
		Local typeParameters:TTypeParameterListSyntaxData = ParseTypeParameterList()
		
		Local extendsKeyword:TSyntaxToken
		Local implementsKeyword:TSyntaxToken
		Local superClass:TTypeSyntaxData
		Local superInterfaces:TTypeListSyntaxData
		
		Local expectMemberImplementations:Int
		Select kind
			Case ETypeKind.Class
				expectMemberImplementations = True
				extendsKeyword = TryTakeToken(TTokenKind.Extends_)
				If extendsKeyword Then
					superClass = ParseSuperType()
					If Not superClass Then
						ReportError "Expected type"
					End If
				End If
				implementsKeyword = TryTakeToken(TTokenKind.Implements_)
				If implementsKeyword Then
					superInterfaces = ParseSuperTypeList()
					If Not superInterfaces.elements Then
						ReportError "Expected type"
					End If
				End If
			Case ETypeKind.Struct_
				expectMemberImplementations = True
				If currentToken.Kind() = TTokenKind.Extends_ Or currentToken.Kind() = TTokenKind.Implements_ Then
					ReportError "Structs cannot extend or implement other types"
					' TODO: how to not lose tokens if parsed here?
				End If
			Case ETypeKind.Interface_
				expectMemberImplementations = False
				extendsKeyword = TryTakeToken(TTokenKind.Extends_)
				If extendsKeyword Then
					superInterfaces = ParseSuperTypeList()
					If Not superInterfaces Then
						ReportError "Expected type"
						superInterfaces = TTypeListSyntaxData.Create([])
					End If
				End If
			Default RuntimeError "Missing case"
		End Select
		
		Local modifiers:TTypeModifierSyntaxData[]
		Repeat
			Local modifierToken:TSyntaxToken = TryTakeToken([TTokenKind.Abstract_, TTokenKind.Final_]) ' NoDebug
			If modifierToken Then modifiers :+ [TTypeModifierSyntaxData.Create(modifierToken)] Else Exit
		Forever
		
		Local metaData:TMetaDataSyntaxData = ParseMetaData()
		
		Local members:ICodeBlockElementSyntaxData[] ' TODO: unify this with the parsing of code blocks, they can also contain visibility modifiers
		Repeat
			members :+ ParseStatementSeparators()
			Local member:ICodeBlockElementSyntaxData = ParseTypeMember(expectMemberImplementations)
			If member Then members :+ [member] Else Exit
		Forever
		Local body:TCodeBlockSyntaxData = TCodeBlockSyntaxData.Create(members)
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(terminatorTokenKind)
		
		Select kind
			Case ETypeKind.Class Return TClassDeclarationSyntaxData.Create(initiatorKeyword, name, typeParameters, extendsKeyword, superClass, implementsKeyword, superInterfaces, modifiers, metaData, body, terminatorKeyword)
			Case ETypeKind.Struct_ Return TStructDeclarationSyntaxData.Create(initiatorKeyword, name, typeParameters, Null, Null, modifiers, metaData, body, terminatorKeyword)
			Case ETypeKind.Interface_ Return TInterfaceDeclarationSyntaxData.Create(initiatorKeyword, name, typeParameters, extendsKeyword, superInterfaces, modifiers, metaData, body, terminatorKeyword)
			Default RuntimeError "Missing case"
		End Select
	End Method
	
	Method ParseExternClassDeclaration:TExternClassDeclarationSyntaxData()
		Throw "TODO"
	End Method
	
	Method ParseExternStructDeclaration:TExternTypeDeclarationSyntaxData()
		Throw "TODO"
	End Method
	
	Method ParseEnumDeclaration:TTypeDeclarationSyntaxData()
		'Local kind:ETypeKind = ETypeKind.Enum_
		Local declarationTokenKind:TTokenKind = TTokenKind.Enum_
		Local terminatorTokenKind:TTokenKind = TTokenKind.EndEnum_
		
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(declarationTokenKind)
		If Not initiatorKeyword Then Return Null
		
		Local name:TNameSyntaxData = ParseName()
		If Not name Then
			ReportError "Expected name"
			name = GenerateMissingName()
		End If
		
		Local baseType:TTypeSyntaxData = ParseEnumBaseType()
		
		Local flagsKeyword:TContextualKeywordSyntaxData = ParseContextualKeyword("Flags")
		
		Local metaData:TMetaDataSyntaxData = ParseMetaData()
		
		Local members:IEnumMemberSyntaxData[]
		Repeat
			members :+ ParseStatementSeparators()
			Local member:TEnumMemberDeclarationSyntaxData = ParseEnumMemberDeclaration()
			If member Then members :+ [member] Else Exit
		Forever
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(terminatorTokenKind)
		
		Return TEnumDeclarationSyntaxData.Create(initiatorKeyword, name, baseType, flagsKeyword, metaData, members, terminatorKeyword)
	End Method
	
	Method ParseEnumMemberDeclaration:TEnumMemberDeclarationSyntaxData()
		Local name:TNameSyntaxData = ParseName()
		If Not name Then Return Null
		
		Local assignment:TAssignmentSyntaxData = ParseAssignment(EAssignmentMode.Constant)
		
		Return TEnumMemberDeclarationSyntaxData.Create(name, assignment)
	End Method
	
	Method ParseExternTypeDeclaration:TExternTypeDeclarationSyntaxData()
		Local externTypeDeclaration:TExternTypeDeclarationSyntaxData
		externTypeDeclaration = ParseExternClassDeclaration();  If externTypeDeclaration Then Return externTypeDeclaration
		externTypeDeclaration = ParseExternStructDeclaration(); If externTypeDeclaration Then Return externTypeDeclaration
		Return Null
	End Method
	
	Method ParseTypeMember:ICodeBlockElementSyntaxData(expectImplementation:Int)
		Local typeMember:ICodeBlockElementSyntaxData
		typeMember = ParseVisibilityDirective();                               If typeMember Then Return typeMember
		
		typeMember = ParseTypeDeclaration();                                   If typeMember Then Return typeMember
		typeMember = ParseTypeMemberCallableDeclaration(expectImplementation); If typeMember Then Return typeMember
		typeMember = ParseTypeMemberVariableDeclaration();                     If typeMember Then Return typeMember
		typeMember = ParseTypeAliasDeclaration();                              If typeMember Then Return typeMember
		Return Null
	End Method
	
	Method ParseExternTypeMember:TExternDeclarationSyntaxData()
		Throw "TODO"
		
		Local externTypeMember:TExternDeclarationSyntaxData
		'externTypeMember = ParseExternTypeDeclaration();     If externTypeMember Then Return externTypeMember
		externTypeMember = ParseExternFunctionDeclaration(); If externTypeMember Then Return externTypeMember
		externTypeMember = ParseExternVariableDeclaration(); If externTypeMember Then Return externTypeMember
		Return Null
	End Method
	
	Method ParseVisibilityDirective:TVisibilityDirectiveSyntaxData()
		Local visibility:TSyntaxToken = TryTakeToken([TTokenKind.Public_, TTokenKind.Protected_, TTokenKind.Private_])
		If Not visibility Then Return Null
		
		Return  TVisibilityDirectiveSyntaxData.Create(visibility)
	End Method
	
	Method ParseCallableDeclaration:TCallableDeclarationSyntaxData()
		Return ParseTypeMemberCallableDeclaration(True) ' TODO: non-member functions cannot be abstract
	End Method
	
	Method ParseTypeMemberCallableDeclaration:TCallableDeclarationSyntaxData(expectImplementation:Int)
		Local kind:ECallableKind
		Local declarationTokenKind:TTokenKind
		Local terminatorTokenKind:TTokenKind
		Select currentToken.Kind()
			Case TTokenKind.Function_ kind = ECallableKind.Function_; declarationTokenKind = TTokenKind.Function_; terminatorTokenKind = TTokenKind.EndFunction_
			Case TTokenKind.Method_   kind = ECallableKind.Method_;   declarationTokenKind = TTokenKind.Method_;   terminatorTokenKind = TTokenKind.EndMethod_
			Default Return Null
		End Select
		
		Local initiatorKeyword:TSyntaxToken = TakeToken(declarationTokenKind)
		
		Local operatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Operator_)
		
		Local name:TCallableDeclarationNameSyntaxData
		Local typeParameters:TTypeParameterListSyntaxData
		
		If operatorKeyword Then
			Local operatorName:TOperatorSyntaxData
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Eq])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Neq])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Lt])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Gt])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Leq])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Geq])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.BitOr])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.BitNot])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.BitAnd])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Plus])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Minus])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Mul])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Div])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Mod_])
			'If Not operatorName Then operatorName = ParseOperator([TTokenKind.Shl_])
			'If Not operatorName Then operatorName = ParseOperator([TTokenKind.Shr_])
			'If Not operatorName Then operatorName = ParseOperator([TTokenKind.Sar_])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.Pow])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.LBracket, TTokenKind.RBracket, TTokenKind.Eq])
			If Not operatorName Then operatorName = ParseOperator([TTokenKind.LBracket, TTokenKind.RBracket])
			If operatorName Then
				name = TCallableDeclarationNameSyntaxData.Create(Null, Null, operatorName)
			Else
				ReportError "Expected overloadable operator"
				name = TCallableDeclarationNameSyntaxData.Create(GenerateMissingName(), Null, Null)
			End If
		Else
			Local keywordName:TSyntaxToken = TryTakeToken([TTokenKind.New_, TTokenKind.Delete_])
			If keywordName Then
				name = TCallableDeclarationNameSyntaxData.Create(Null, keywordName, Null)
			Else
				Local identifierName:TNameSyntaxData = ParseName()
				If identifierName Then
					name = TCallableDeclarationNameSyntaxData.Create(identifierName, Null, Null)
				Else
					ReportError "Expected " + initiatorKeyword.lexerToken.value.ToLower() + " name"
					name = TCallableDeclarationNameSyntaxData.Create(GenerateMissingName(), Null, Null)
				End If
				
				typeParameters = ParseTypeParameterList()
			End If
		End If
		
		Local type_:TTypeSyntaxData = ParseVariableDeclaratorType(EArrayDimensionsOption.Disallow)
		' generate a void-return-no-parameters type if no type can be parsed;
		' just append an empty parameter list if a type was parsed but it isn't a callable type
		If Not type_ Then
			ReportError "Expected callable type"
			type_ = TTypeSyntaxData.Create(Null, Null, Null, Null, [TCallableTypeSuffixSyntaxData.Create(GenerateMissingToken(TTokenKind.LParen), TVariableDeclarationSyntaxData.Create(Null, [], TVariableDeclaratorListSyntaxData.Create([]), Null), GenerateMissingToken(TTokenKind.RParen))])
		Else If Not type_.suffixes Or Not (TCallableTypeSuffixSyntaxData(type_.suffixes[type_.suffixes.length - 1])) Then
			ReportError "Expected callable type"
			type_ = TTypeSyntaxData.Create(type_.colon, type_.base, Null, Null, type_.suffixes + [TCallableTypeSuffixSyntaxData.Create(TakeToken(TTokenKind.LParen), TVariableDeclarationSyntaxData.Create(Null, [], TVariableDeclaratorListSyntaxData.Create([]), Null), TakeToken(TTokenKind.RParen))]) ' TODO: do not use TakeToken here to avoid duplicate errors?
		End If
		
		Local modifiers:TCallableModifierSyntaxData[]
		Repeat
			Local modifierToken:TSyntaxToken = TryTakeToken([TTokenKind.Abstract_, TTokenKind.Final_, TTokenKind.Override_, TTokenKind.Default_, TTokenKind.Inline_]) ' NoDebug
			If modifierToken Then
				modifiers :+ [TCallableModifierSyntaxData.Create(modifierToken)]
			Else
				Exit
			End If
		Forever
		
		Local metaData:TMetaDataSyntaxData = ParseMetaData()
		
		Local body:TCodeBlockSyntaxData
		Local terminatorKeyword:TSyntaxToken
		If (expectImplementation And Not ContainsKind(modifiers, TTokenKind.Abstract_)) Or (Not expectImplementation And ContainsKind(modifiers, TTokenKind.Default_)) Then
			body = ParseCodeBlock([terminatorTokenKind])
			terminatorKeyword = TakeToken(terminatorTokenKind)
		End If
		
		Return TCallableDeclarationSyntaxData.Create(initiatorKeyword, operatorKeyword, name, typeParameters, type_, modifiers, metaData, body, terminatorKeyword)
		
		Function ContainsKind:Int(array:TCallableModifierSyntaxData[], modifierKind:TTokenKind)
			For Local m:TCallableModifierSyntaxData = EachIn array
				If m.token.Kind() = modifierKind Then Return True
			Next
			Return False
		End Function
	End Method
	
	Method ParseExternFunctionDeclaration:TExternFunctionDeclarationSyntaxData()
		Local kind:ECallableKind
		Local declarationTokenKind:TTokenKind
		Select currentToken.Kind()
			Case TTokenKind.Function_ kind = ECallableKind.Function_; declarationTokenKind = TTokenKind.Function_
			Default Return Null
		End Select
		
		Local initiatorKeyword:TSyntaxToken = TakeToken(declarationTokenKind)
		
		Local name:TCallableDeclarationNameSyntaxData
		Local identifierName:TNameSyntaxData = ParseName()
		If identifierName Then
			name = TCallableDeclarationNameSyntaxData.Create(identifierName, Null, Null)
		Else
			ReportError "Expected " + initiatorKeyword.lexerToken.value.ToLower() + " name"
			name = TCallableDeclarationNameSyntaxData.Create(GenerateMissingName(), Null, Null)
		End If
		
		' TODO type params
		
		Local type_:TTypeSyntaxData = ParseVariableDeclaratorType(EArrayDimensionsOption.Disallow)
		' generate a void-return-no-parameters type if no type can be parsed;
		' just append an empty parameter list if a type was parsed but it isn't a callable type
		If Not type_ Then
			ReportError "Expected callable type"
			type_ = TTypeSyntaxData.Create(Null, Null, Null, Null, [TCallableTypeSuffixSyntaxData.Create(GenerateMissingToken(TTokenKind.LParen), TVariableDeclarationSyntaxData.Create(Null, [], TVariableDeclaratorListSyntaxData.Create([]), Null), GenerateMissingToken(TTokenKind.RParen))])
		Else If Not type_.suffixes Or Not (TCallableTypeSuffixSyntaxData(type_.suffixes[type_.suffixes.length - 1])) Then
			ReportError "Expected callable type"
			type_ = TTypeSyntaxData.Create(type_.colon, type_.base, Null, Null, type_.suffixes + [TCallableTypeSuffixSyntaxData.Create(TakeToken(TTokenKind.LParen), TVariableDeclarationSyntaxData.Create(Null, [], TVariableDeclaratorListSyntaxData.Create([]), Null), TakeToken(TTokenKind.RParen))]) ' TODO: do not use TakeToken here to avoid duplicate errors?
		End If
		
		Local externSignatureAssignment:TExternSignatureAssignmentSyntaxData = ParseExternSignatureAssignment()
		
		Local metaData:TMetaDataSyntaxData = ParseMetaData()
		
		Return TExternFunctionDeclarationSyntaxData.Create(initiatorKeyword, name, type_, externSignatureAssignment, metaData)
	End Method
	
	Method ParseVariableDeclaration:TVariableDeclarationSyntaxData(acceptedDeclarationKeywords:TTokenKind[], multipleDeclaratorsOption:EMultipleDeclaratorsOption, initializersOption:EInitializersOption)
		' always succeeds if acceptedDeclarationKeywords contains Null
		' if acceptedDeclarationKeywords contains a Null element, a declaration will be accepted without any declaration keyword
		' in the case of a Const declaration being parsed, initializersOption is ignored and treated as Require
		Assert acceptedDeclarationKeywords Else "Must specify accepted keywords"
		? Debug
		For Local i:Int = 0 Until acceptedDeclarationKeywords.length
			If Not acceptedDeclarationKeywords[i] Then Continue ' Select is broken on types with overloaded = in NG
			Select acceptedDeclarationKeywords[i]
				Case TTokenKind.Local_, TTokenKind.Global_, TTokenKind.Const_, TTokenKind.Field_', Null ' ok
				Default RuntimeError "Unsupported declaration keyword"
			End Select
			Assert Not (acceptedDeclarationKeywords[i] = TTokenKind.Const_ And initializersOption = EInitializersOption.Disallow) Else "Cannot disallow initializers when Const is accepted"
		Next
		?
		
		Local allowNoDeclarationKeyword:Int = False
		
		Local declarationKeyword:TSyntaxToken
		For Local i:Int = 0 Until acceptedDeclarationKeywords.length
			If Not acceptedDeclarationKeywords[i] Then
				allowNoDeclarationKeyword = True ' accept declaration without any declaration keyword
			Else If Not declarationKeyword Then
				declarationKeyword = TryTakeToken(acceptedDeclarationKeywords[i])
			End If
		Next
		If Not declarationKeyword And Not allowNoDeclarationKeyword Then Return Null
		
		Local modifiers:TVariableModifierSyntaxData[]
		Repeat
			Local modifierToken:TSyntaxToken = TryTakeToken(TTokenKind.ReadOnly_)
			If modifierToken Then
				modifiers :+ [TVariableModifierSyntaxData.Create(modifierToken)]
			Else
				Exit
			End If
		Forever
		If declarationKeyword And declarationKeyword.Kind() = TTokenKind.Const_ Then
			initializersOption = EInitializersOption.Require ' always require an initializer for Const declarations
		End If
		
		Local declarators:TVariableDeclaratorListSyntaxData
		If multipleDeclaratorsOption = EMultipleDeclaratorsOption.Allow Then
			declarators = ParseVariableDeclaratorList(initializersOption)
		Else
			' TODO: option for optional variable names
			'       since a parameter named x of type () is syntactically indistinguishable
			'       from an unnamed parameter of type x(), declarators in a declaration should
			'       be either all named or all unnamed to reduce the risk of ambiguities;
			'       it is however still possible for a declaration as a whole to be ambiguous if
			'       all of its declarators are ambiguous - in this case semantic analysis could
			'       first look up x as a type name, and, if unsuccessful, interpret it as a variable
			'       name; this would break backwards compatibility, and while unlikely to affect
			'       existing code since variables names and type names follow different conventions,
			'       it could lead to unexpected behaviour in case of a misspelled type name
			Local declarator:TVariableDeclaratorSyntaxData = ParseVariableDeclarator(initializersOption)
			If declarator Then
				declarators = TVariableDeclaratorListSyntaxData.Create([TVariableDeclaratorListElementSyntaxData.Create(Null, declarator)])
			Else
				ReportError "Expected variable declarator"
				declarators = TVariableDeclaratorListSyntaxData.Create([])
			End If
		End If
		
		Local metaData:TMetaDataSyntaxData = ParseMetaData() ' TODO: not for locals (except params?)
		
		Return TVariableDeclarationSyntaxData.Create(declarationKeyword, modifiers, declarators, metaData)
	End Method
	
	Method ParseVariableDeclarator:TVariableDeclaratorSyntaxData(initializersOption:EInitializersOption)
		Local name:TNameSyntaxData = ParseName()
		If Not name Then Return Null
		
		Local arrayDimensionsOption:EArrayDimensionsOption
		Select initializersOption
			Case EInitializersOption.Disallow arrayDimensionsOption = EArrayDimensionsOption.Disallow
			Case EInitializersOption.Allow    arrayDimensionsOption = EArrayDimensionsOption.Allow
			Case EInitializersOption.Require  arrayDimensionsOption = EArrayDimensionsOption.Require
			Default RuntimeError "Missing case"
		End Select
		
		Local type_:TTypeSyntaxData = ParseVariableDeclaratorType(arrayDimensionsOption)
		If Not type_ Then
			ReportError "Expected type"
		End If
		
		Local initializer:TAssignmentSyntaxData
		Local typeHasArrayDimensions:Int = type_ And type_.suffixes And HasAnyArrayDimensions(type_.suffixes[type_.suffixes.length - 1])
		If initializersOption <> EInitializersOption.Disallow And Not typeHasArrayDimensions Then ' dimensions act as an initializer; cannot have both
			initializer = ParseAssignment(EAssignmentMode.Regular)
		End If
		If Not initializer And initializersOption = EInitializersOption.Require Then
			ReportError "Expected initializer"
			initializer = TAssignmentSyntaxData.Create(TOperatorSyntaxData.Create([GenerateMissingToken(TTokenKind.Eq)]), GenerateMissingExpression())
		End If
		
		If Not type_ And Not initializer Then
			type_ = GenerateMissingType()
			' if the type is missing but an initializer is present, it can be treated as an inference
			' assignment (the type of the variable will later be inferred from the type of the expression)
		End If
		
		Return TVariableDeclaratorSyntaxData.Create(name, type_, initializer)
	End Method
	
	'Method ParseTypeParameterDeclaration:TTypeParameterDeclarationSyntaxData() ' always succeeds
	'	Local declarators:TVariableDeclaratorListSyntaxData = ParseTypeParameterDeclaratorList()
	'	If Not declarators.elements Then
	'		ReportError "Expected type parameter declaration"
	'	End If
	'	
	'	Return TTypeParameterDeclarationSyntaxData.Create(declarators)
	'End Method
	
	Method ParseTypeParameterDeclarator:TTypeParameterDeclaratorSyntaxData()
		Local name:TNameSyntaxData = ParseName()
		If Not name Then Return Null
		
		Return TTypeParameterDeclaratorSyntaxData.Create(name)
	End Method
	
	Method ParseCodeBlockVariableDeclaration:TVariableDeclarationSyntaxData()
		Return ParseVariableDeclaration([TTokenKind.Local_, TTokenKind.Global_, TTokenKind.Const_], EMultipleDeclaratorsOption.Allow, EInitializersOption.Allow)
	End Method
	
	Method ParseParameterVariableDeclaration:TVariableDeclarationSyntaxData()
		Return ParseVariableDeclaration([TTokenKind(Null)], EMultipleDeclaratorsOption.Allow, EInitializersOption.Allow)
	End Method
	
	Method ParseTypeMemberVariableDeclaration:TVariableDeclarationSyntaxData()
		Return ParseVariableDeclaration([TTokenKind.Field_, TTokenKind.Global_, TTokenKind.Const_], EMultipleDeclaratorsOption.Allow, EInitializersOption.Allow)
	End Method
	
	Method ParseForCounterVariableDeclaration:TVariableDeclarationSyntaxData()
		Return ParseVariableDeclaration([TTokenKind.Local_], EMultipleDeclaratorsOption.Disallow, EInitializersOption.Disallow)
	End Method
	
	Method ParseCatchVariableDeclaration:TVariableDeclarationSyntaxData()
		Return ParseVariableDeclaration([TTokenKind(Null)], EMultipleDeclaratorsOption.Disallow, EInitializersOption.Disallow)
	End Method
	
	Method ParseExternVariableDeclaration:TExternVariableDeclarationSyntaxData()
		Throw "TODO" ' TODO
	End Method
	
	Method ParseTypeAliasDeclaration:IDeclarationSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Alias_)
		If Not keyword Then Return Null
		
		Throw "TODO" ' TODO
	End Method
	
	Method ParseLabelDeclaration:TLabelDeclarationSyntaxData()
		Local hash:TSyntaxToken = TryTakeToken(TTokenKind.FloatSigil)
		If Not hash Then Return Null
		
		Local name:TNameSyntaxData = ParseName()
		If Not name Then
			ReportError "Expected name"
			name = GenerateMissingName()
		End If
		
		Return TLabelDeclarationSyntaxData.Create(hash, name)
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Statements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseStatement:IStatementSyntaxData()
		Local statement:IStatementSyntaxData
		
		statement = ParseIfStatement();              If statement Then Return statement
		statement = ParseSelectStatement();          If statement Then Return statement
		statement = ParseForStatement();             If statement Then Return statement
		statement = ParseWhileStatement();           If statement Then Return statement
		statement = ParseRepeatStatement();          If statement Then Return statement
		statement = ParseExitStatement();            If statement Then Return statement
		statement = ParseContinueStatement();        If statement Then Return statement
		statement = ParseReturnStatement();          If statement Then Return statement
		statement = ParseGotoStatement();            If statement Then Return statement
		statement = ParseTryStatement();             If statement Then Return statement
		statement = ParseThrowStatement();           If statement Then Return statement
		statement = ParseAssertStatement();          If statement Then Return statement
		statement = ParseEndStatement();             If statement Then Return statement
		
		statement = ParseExpressionBasedStatement(); If statement Then Return statement
		
		statement = ParseNativeCodeStatement();      If statement Then Return statement
		
		statement = ParseDefDataStatement();         If statement Then Return statement
		statement = ParseReadDataStatement();        If statement Then Return statement
		statement = ParseRestoreDataStatement();     If statement Then Return statement
		
		statement = ParseReleaseStatement();         If statement Then Return statement
		
		Return Null
	End Method
	
	Method ParseIfStatement:TIfStatementSyntaxData()
		Local ifKeyword:TSyntaxToken = TryTakeToken(TTokenKind.If_)
		If Not ifKeyword Then Return Null
		
		Local condition:IExpressionSyntaxData = ParseExpression()
		If Not condition Then
			ReportError "Expected expression"
			condition = GenerateMissingExpression()
		End If
		
		Local thenKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Then_)
		
		Local parseIfBranchBody:TCodeBlockSyntaxData(self_:TParser)
		Local isSingleLineIf:Int = Not (currentToken.Kind() = TTokenKind.Linebreak Or currentToken.Kind() = TTokenKind.Semicolon)
		If isSingleLineIf Then
			Function ParseSingleLineIfBranchBody:TCodeBlockSyntaxData(self_:TParser)
				Local blockElements:ICodeBlockElementSyntaxData[]
				Repeat
					Local separator:TStatementSeparatorSyntaxData = self_.ParseStatementSeparator()
					If separator Then
						blockElements :+ [separator]
						If separator.token.Kind() = TTokenKind.Linebreak Then Exit
					Else
						Local statement:IStatementSyntaxData = self_.ParseStatement()
						If statement Then
							blockElements :+ [statement]
						Else
							Exit
						End If
					End If
				Forever
				Return TCodeBlockSyntaxData.Create(blockElements)
			End Function
			parseIfBranchBody = ParseSingleLineIfBranchBody
		Else
			Function ParseMultiLineIfBranchBody:TCodeBlockSyntaxData(self_:TParser)
				Return self_.ParseCodeBlock([TTokenKind.Else_, TTokenKind.ElseIf_, TTokenKind.EndIf_])
			End Function
			parseIfBranchBody = ParseMultiLineIfBranchBody
		End If
		
		Local thenBody:TCodeBlockSyntaxData = parseIfBranchBody(Self)
		Local thenBranch:TThenIfBranchSyntaxData = TThenIfBranchSyntaxData.Create(thenKeyword, thenBody)
		
		Local branches:TIfBranchSyntaxData[] = [thenBranch]
		Repeat
			Select currentToken.Kind()
				Case TTokenKind.ElseIf_
					Local elseIfKeyword:TSyntaxToken = TakeToken()
					Local condition:IExpressionSyntaxData = ParseExpression()
					If Not condition Then
						ReportError "Expected expression"
						condition = GenerateMissingExpression()
					End If
					Local thenKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Then_)
					Local body:TCodeBlockSyntaxData = parseIfBranchBody(Self)
					branches :+ [TElseIfIfBranchSyntaxData.Create(elseIfKeyword, condition, thenKeyword, body)]
				Case TTokenKind.Else_
					Local elseKeyword:TSyntaxToken = TakeToken()
					Local body:TCodeBlockSyntaxData = parseIfBranchBody(Self)
					branches :+ [TElseIfBranchSyntaxData.Create(elseKeyword, body)]
				Default Exit
			End Select
		Forever
		
		Local endIfKeyword:TSyntaxToken
		If Not isSingleLineIf Then endIfKeyword = TakeToken(TTokenKind.EndIf_)
		
		Return TIfStatementSyntaxData.Create(ifKeyword, condition, branches, endIfKeyword)
	End Method
	
	Method ParseSelectStatement:TSelectStatementSyntaxData()
		Local selectKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Select_)
		If Not selectKeyword Then Return Null
		
		Local expression:IExpressionSyntaxData = ParseExpression()
		If Not expression Then
			ReportError "Expected expression"
			expression = GenerateMissingExpression()
		End If
		
		Local statementSeparators:TStatementSeparatorSyntaxData[] = ParseStatementSeparators()
		
		Local branches:TSelectBranchSyntaxData[]
		Repeat
			Select currentToken.Kind()
				Case TTokenKind.Case_
					Local keyword:TSyntaxToken = TakeToken()
					Local expressionList:TExpressionListSyntaxData = ParseExpressionList(EEmptyElementsOption.Disallow)
					If Not expressionList.elements Then
						ReportError "Expected expression"
					End If
					Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Case_, TTokenKind.Default_, TTokenKind.EndSelect_])
					branches :+ [TCaseSelectBranchSyntaxData.Create(keyword, expressionList, body)]
				Case TTokenKind.Default_
					Local keyword:TSyntaxToken = TakeToken()
					Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Case_, TTokenKind.Default_, TTokenKind.EndSelect_])
					branches :+ [TDefaultSelectBranchSyntaxData.Create(keyword, body)]
				Default Exit
			End Select
		Forever
		
		Local endSelectKeyword:TSyntaxToken = TakeToken(TTokenKind.EndSelect_)
		
		Return TSelectStatementSyntaxData.Create(selectKeyword, expression, statementSeparators, branches, endSelectKeyword)
	End Method
	
	Method ParseForStatement:TForStatementSyntaxData()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.For_)
		If Not initiatorKeyword Then Return Null
		
		Local counter:TForCounterSyntaxData
			Local counterDeclaration:TVariableDeclarationSyntaxData = ParseForCounterVariableDeclaration()
		If counterDeclaration Then
			counter = TForCounterDeclarationSyntaxData.Create(counterDeclaration)
		Else
			Local counterExpression:IExpressionSyntaxData = ParseLValue()
			If counterExpression Then
				counter = TForCounterExpressionSyntaxData.Create(counterExpression)
			Else
				ReportError "Expected declaration or expression"
				counter = TForCounterExpressionSyntaxData.Create(GenerateMissingExpression())
			End If
		End If
		'
		'TODO: add expectedTerminators parameter To ParseExpression?
		Local eq:TSyntaxToken = SkipAndTakeToken(TTokenKind.Eq, StatementSeparatorTokenKinds)
		
		Local valueSequence:TForValueSequenceSyntaxData
		If currentToken.Kind() = TTokenKind.EachIn_ Then
			Local eachInKeyword:TSyntaxToken = TakeToken()
			Local iterableExpression:IExpressionSyntaxData = ParseExpression()
			If Not iterableExpression Then
				ReportError "Expected expression"
				iterableExpression = GenerateMissingExpression()
			End If
			valueSequence = TForEachInValueSequenceSyntaxData.Create(eachInKeyword, iterableExpression)
		Else
			Local expressionAfterEq:IExpressionSyntaxData = ParseExpression()
			'Local skippedTokens:TSyntaxToken[]
			'While Not expressionAfterEq And Not terminatorStack.Contains(currentToken.Kind())
			'	skippedTokens :+ [TakeToken()]
			'	ReportError "skipped: " + skippedTokens[skippedTokens.length - 1].lexerToken.value
			'	expressionAfterEq = ParseExpression()
			'Wend
			If expressionAfterEq Then
				Local startExpression:IExpressionSyntaxData = expressionAfterEq
				If Not startExpression Then
					ReportError "Expected expression"
					startExpression = GenerateMissingExpression()
				End If
				
				Local keyword:TSyntaxToken = TryTakeToken([TTokenKind.To_, TTokenKind.Until_])
				If Not keyword Then
					ReportError "Expected To or Until"
					keyword = GenerateMissingToken(TTokenKind.To_)
				End If
				
				Local endExpression:IExpressionSyntaxData = ParseExpression()
				If Not endExpression Then
					ReportError "Expected expression"
					endExpression = GenerateMissingExpression()
				End If
				
				Select keyword.Kind()
					Case TTokenKind.To_    valueSequence = TForToValueSequenceSyntaxData.Create(startExpression, keyword, endExpression)
					Case TTokenKind.Until_ valueSequence = TForUntilValueSequenceSyntaxData.Create(startExpression, keyword, endExpression)
					Default RuntimeError "Missing case"
				End Select
			Else
				ReportError "Expected EachIn or expression"
				valueSequence = TForEachInValueSequenceSyntaxData.Create(GenerateMissingToken(TTokenKind.EachIn_), GenerateMissingExpression())
			End If
		End If
		
		Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Next_])
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(TTokenKind.Next_)
				
		Return TForStatementSyntaxData.Create(initiatorKeyword, counter, eq, valueSequence, body, terminatorKeyword)
	End Method
	
	Method ParseWhileStatement:TWhileStatementSyntaxData()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.While_)
		If Not initiatorKeyword Then Return Null
		
		Local condition:IExpressionSyntaxData = ParseExpression()
		If Not condition Then
			ReportError "Expected expression"
			condition = GenerateMissingExpression()
		End If
		
		Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Wend_])
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(TTokenKind.Wend_)
		
		Return TWhileStatementSyntaxData.Create(initiatorKeyword, condition, body, terminatorKeyword)
	End Method
	
	Method ParseRepeatStatement:IStatementSyntaxData()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Repeat_)
		If Not initiatorKeyword Then Return Null
		
		Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Until_, TTokenKind.Forever_])
		
		Local terminator:TRepeatTerminatorSyntaxData
		Local terminatorKeyword:TSyntaxToken = TryTakeToken([TTokenKind.Until_, TTokenKind.Forever_])
		If Not terminatorKeyword Then
			ReportError "Expected Until or Forever"
			terminatorKeyword = GenerateMissingToken(TTokenKind.Forever_)
		End If
		Select terminatorKeyword.Kind()
			Case TTokenKind.Until_
				Local condition:IExpressionSyntaxData = ParseExpression()
				If Not condition Then
					ReportError "Expected expression"
					condition = GenerateMissingExpression()
				End If
				terminator = TRepeatUntilTerminatorSyntaxData.Create(terminatorKeyword, condition)
			Case TTokenKind.Forever_
				terminator = TRepeatForeverTerminatorSyntaxData.Create(terminatorKeyword)
			Default RuntimeError "Missing case"
		End Select
		
		Return TRepeatStatementSyntaxData.Create(initiatorKeyword, body, terminator)
	End Method
	
	Method ParseExitStatement:TExitStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Exit_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntaxData = ParseName()
		
		Return TExitStatementSyntaxData.Create(keyword, labelName)
	End Method
	
	Method ParseContinueStatement:TContinueStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Continue_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntaxData = ParseName()
		
		Return TContinueStatementSyntaxData.Create(keyword, labelName)
	End Method
	
	Method ParseGotoStatement:TGotoStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Goto_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntaxData = ParseName()
		If Not labelName Then
			ReportError "Expected label name"
			labelName = GenerateMissingName()
		End If
		
		Return TGotoStatementSyntaxData.Create(keyword, labelName)
	End Method
	
	Method ParseReturnStatement:TReturnStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Return_)
		If Not keyword Then Return Null
		
		Local expression:IExpressionSyntaxData = ParseExpression()
		
		Return TReturnStatementSyntaxData.Create(keyword, expression)
	End Method
	
	Method ParseTryStatement:TTryStatementSyntaxData()
		Local tryKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Try_)
		If Not tryKeyword Then Return Null
		
		Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Catch_, TTokenKind.Finally_, TTokenKind.EndTry_])
		
		Local branches:TTryBranchSyntaxData[]
		Repeat
			Select currentToken.Kind()
				Case TTokenKind.Catch_
					Local keyword:TSyntaxToken = TakeToken()
					Local declaration:TVariableDeclarationSyntaxData = ParseCatchVariableDeclaration()
					If Not declaration Then
						ReportError "Expected variable declaration"
						declaration = TVariableDeclarationSyntaxData.Create(Null, [], TVariableDeclaratorListSyntaxData.Create([TVariableDeclaratorListElementSyntaxData.Create(Null, TVariableDeclaratorSyntaxData.Create(GenerateMissingName(), Null, Null))]), Null)
					End If
					Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Catch_, TTokenKind.Finally_, TTokenKind.EndTry_])
					branches :+ [TCatchTryBranchSyntaxData.Create(keyword, declaration, body)]
				Case TTokenKind.Finally_
					Local keyword:TSyntaxToken = TakeToken()
					Local body:TCodeBlockSyntaxData = ParseCodeBlock([TTokenKind.Catch_, TTokenKind.Finally_, TTokenKind.EndTry_])
					branches :+ [TFinallyTryBranchSyntaxData.Create(keyword, body)]
				Default Exit
			End Select
		Forever
		If Not branches Then ReportError "Expected Catch or Finally"
		
		Local endTryKeyword:TSyntaxToken = TakeToken(TTokenKind.EndTry_)
				
		Return TTryStatementSyntaxData.Create(tryKeyword, body, branches, endTryKeyword)
	End Method
	
	Method ParseThrowStatement:TThrowStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Throw_)
		If Not keyword Then Return Null
		
		Local expression:IExpressionSyntaxData = ParseExpression()
		If Not expression Then
			ReportError "Expected expression"
			expression = GenerateMissingExpression()
		End If
		
		Return TThrowStatementSyntaxData.Create(keyword, expression)
	End Method
	
	Method ParseAssertStatement:TAssertStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Assert_)
		If Not keyword Then Return Null
		
		Local expression:IExpressionSyntaxData = ParseExpression()
		If Not expression Then
			ReportError "Expected expression"
			expression = GenerateMissingExpression()
		End If
		
		Local commaOrElse:TSyntaxToken = TryTakeToken([TTokenKind.Comma, TTokenKind.Else_])
		If Not commaOrElse Then
			ReportError "Expected comma or Else"
			commaOrElse = GenerateMissingToken(TTokenKind.Else_)
		End If
		
		Local message:TStringLiteralExpressionSyntaxData
		If Not message Then
			ReportError "Expected comma or Else"
			message = TStringLiteralExpressionSyntaxData.Create(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
		End If
		
		Return TAssertStatementSyntaxData.Create(keyword, expression, commaOrElse, message)
	End Method
	
	Method ParseEndStatement:TEndStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.End_)
		If Not keyword Then Return Null
		
		Return TEndStatementSyntaxData.Create(keyword)
	End Method
	
	Method ParseNativeCodeStatement:TNativeCodeStatementSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.NativeCode)
		If Not token Then Return Null
		
		Return TNativeCodeStatementSyntaxData.Create(token)
	End Method
	
	Method ParseDefDataStatement:TDefDataStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.DefData_)
		If Not keyword Then Return Null
		
		Local expressionList:TExpressionListSyntaxData = ParseExpressionList(EEmptyElementsOption.Disallow)
		
		Return TDefDataStatementSyntaxData.Create(keyword, expressionList)
	End Method
	
	Method ParseReadDataStatement:TReadDataStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.ReadData_)
		If Not keyword Then Return Null
		
		Local expressionList:TExpressionListSyntaxData = ParseExpressionList(EEmptyElementsOption.Disallow)
		
		Return TReadDataStatementSyntaxData.Create(keyword, expressionList)
	End Method
	
	Method ParseRestoreDataStatement:TRestoreDataStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.RestoreData_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntaxData = ParseName()
		If Not labelName Then
			ReportError "Expected label name"
			labelName = GenerateMissingName()
		End If
		
		Return TRestoreDataStatementSyntaxData.Create(keyword, labelName)
	End Method
	
	Method ParseReleaseStatement:TReleaseStatementSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Release_)
		If Not keyword Then Return Null
		
		Local handleExpression:IExpressionSyntaxData = ParseExpression()
		If Not handleExpression Then
			ReportError "Expected expression"
			handleExpression = GenerateMissingExpression()
		End If
		
		Return TReleaseStatementSyntaxData.Create(keyword, handleExpression)
	End Method
	
	Method ParseExpressionBasedStatement:IStatementSyntaxData()
		' TODO: expensive backtracking here, improve
		' the order of these here is very important, see below
		Local expressionBasedStatement:IStatementSyntaxData
		expressionBasedStatement = ParseAssignmentStatement();    If expressionBasedStatement Then Return expressionBasedStatement
		expressionBasedStatement = ParseParenlessCallStatement(); If expressionBasedStatement Then Return expressionBasedStatement
		expressionBasedStatement = ParseExpressionStatement();    If expressionBasedStatement Then Return expressionBasedStatement
		Return Null
	End Method
	
	Method ParseAssignmentStatement:TAssignmentStatementSyntaxData() ' backtracks
		' this method parses code constructs whose first part would also be accepted by ParseParenlessCallStatement,
		' so in any context where both are valid, this method must be called first
		Local state:SParserState = SaveState()
		
		Local target:IExpressionSyntaxData = ParseLValue()
		If Not target Then Return Null
		
		Local assignment:TAssignmentSyntaxData = ParseAssignment(EAssignmentMode.RegularOrCompound)
		If Not assignment Then
			RestoreState state
			Return Null
		End If
		
		Return TAssignmentStatementSyntaxData.Create(target, assignment)
	End Method
	
	Method ParseLValue:IExpressionSyntaxData()
		' this must not attempt to parse a relational expression, otherwise there will be no = left over for the assignment
		Return ParsePostfixCompatibleExpression(False) ' TODO: exclude call expressions
	End Method
	
	Method ParseParenlessCallStatement:TParenlessCallStatementSyntaxData() ' backtracks
		' this method accepts code constructs whose first part would also be accepted by ParseExpressionStatement,
		' so in any context where both are valid, this method must be called first
		Local state:SParserState = SaveState()
		
		Local expression:IExpressionSyntaxData = ParsePostfixCompatibleExpression(True)
		If Not expression Then Return Null
		
		If Not IsValidExpressionType(expression) Then
			' if the expression that was parsed is a call expression, the parser will
			' backtrack here and parse it again as an expression statement;
			' the call expression that was just parsed is not reused in this case, because
			' paren-less call statements and call expressions have different rules as to how
			' "<" and ">" should be parsed, e.g. "a<b> c" contains a type application in case
			' of a paren-less call statement, but two relational operators otherwise);
			' call expressions cannot just be excluded from parsing here though, because they
			' can still be part of a paren-less call statement, e.g. "x().y"
			RestoreState state
			Return Null
		End If
		Function IsValidExpressionType:Int(expression:IExpressionSyntaxData)
			If TMemberAccessExpressionSyntaxData(expression) Then Return True
			If TIndexExpressionSyntaxData(expression) Then Return True
			If TTypeApplicationExpressionSyntaxData(expression) Then Return True
			'If TTypeAssertionExpressionSyntaxData(expression) Then Return Return IsValidType(TTypeAssertionExpressionSyntaxData(expression).expression)
			If TParenExpressionSyntaxData(expression) Then Return IsValidExpressionType(TParenExpressionSyntaxData(expression).expression)
			If TNameExpressionSyntaxData(expression) Then Return True
			Return False
		End Function
		
		Local callOperator:TExpressionListSyntaxData = ParseExpressionList(EEmptyElementsOption.Allow)
		
		Return TParenlessCallStatementSyntaxData.Create(expression, callOperator)
	End Method
	
	Method ParseExpressionStatement:TExpressionStatementSyntaxData() ' backtracks
		Local state:SParserState = SaveState()
		
		Local expression:IExpressionSyntaxData = ParsePostfixCompatibleExpression(False)
		If Not expression Then Return Null
		Assert Not (TMemberAccessExpressionSyntaxData(expression) Or TIndexExpressionSyntaxData(expression) Or TTypeApplicationExpressionSyntaxData(expression)) Else "ParseExpressionStatement parsed an expression type that should have been parsed by ParseParenlessCallStatement" '  Or TTypeAssertionExpressionSyntaxData
		
		If Not IsValidExpressionType(expression) Then
			RestoreState state
			Return Null
		End If
		Function IsValidExpressionType:Int(expression:IExpressionSyntaxData)
			If TCallExpressionSyntaxData(expression) Then Return True
			If TNewExpressionSyntaxData(expression) Then Return True
			If TParenExpressionSyntaxData(expression) Then Return IsValidExpressionType(TParenExpressionSyntaxData(expression).expression)
			Return False
		End Function
		
		Return TExpressionStatementSyntaxData.Create(expression)
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Expressions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseExpression:IExpressionSyntaxData()
		Return ParseRangeCompatibleExpression()
	End Method
	
	Rem
	Method SkipAndParseExpression:IExpressionSyntaxData(expectedTerminatorKinds:TTokenKind[])
		Local expression:IExpressionSyntaxData = ParseExpression()
		If Not expression Then
			Local state:SParserState = SaveState()
			Local skippedTokens:TSyntaxToken[]
			While Not expression And Not terminatorStack.Contains(currentToken.Kind())
				skippedTokens :+ [TakeToken()]
				expression = ParseExpression()
			Wend
			If expression Then
				If skippedTokens Then
					For Local st:TSyntaxToken = EachIn skippedTokens
						ReportError "Unexpected " + st.lexerToken.value
						leadingTrivia :+ st.leadingTrivia + [st.lexerToken] + st.trailingTrivia
					Next
					leadingTrivia :+ token.leadingTrivia
					TODO: ' how to attach skipped trivia to the first token of the expression?
					'token = TSyntaxToken.Create(token.lexerToken, leadingTrivia, token.trailingTrivia)
				End If
			Else
				RestoreState state
			End If
		End If
		Return expression
	End Method
	End Rem
	
	Method ParseRangeCompatibleExpression:IRangeCompatibleExpressionSyntaxData()
		' TODO: even if it means breaking backwards compatibility, this should probably have higher
		'       precedence than logical and relational operators
		'       relational operators other than = and <> should not be supported by ranges,
		'       and logical operators should probably not be supported by structs in general
		'       (assuming they cannot be overloaded someday), so the chances of breaking any
		'       code with that change would be pretty low
		
		' the range operator is unique in that its operands are optional
		Local lhs:IRangeCompatibleExpressionSyntaxData
		If currentToken.Kind() <> TTokenKind.DotDot Then
			lhs = ParseOrCompatibleExpression()
			If Not lhs Then Return Null
		End If
		' lhs may be Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.DotDot])
			If op Then
				Local rhs:IOrCompatibleExpressionSyntaxData = ParseOrCompatibleExpression()
				' rhs may be Null
				
				lhs = TRangeExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseOrCompatibleExpression:IOrCompatibleExpressionSyntaxData()
		Local lhs:IOrCompatibleExpressionSyntaxData = ParseAndCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.Or_])
			If op Then
				Local rhs:IAndCompatibleExpressionSyntaxData = ParseAndCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TOrExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseAndCompatibleExpression:IAndCompatibleExpressionSyntaxData()
		Local lhs:IAndCompatibleExpressionSyntaxData = ParseRelationalCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.And_])
			If op Then
				Local rhs:IRelationalCompatibleExpressionSyntaxData = ParseRelationalCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TAndExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseRelationalCompatibleExpression:IRelationalCompatibleExpressionSyntaxData()
		Local lhs:IRelationalCompatibleExpressionSyntaxData = ParseUnionCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.Eq])
			If Not op Then op = ParseOperator([TTokenKind.Neq])
			If Not op Then op = ParseOperator([TTokenKind.Lt])
			If Not op Then op = ParseOperator([TTokenKind.Gt])
			If Not op Then op = ParseOperator([TTokenKind.Leq])
			If Not op Then op = ParseOperator([TTokenKind.Geq])
			If op Then
				Local rhs:IUnionCompatibleExpressionSyntaxData = ParseUnionCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TRelationalExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	' alternative implementation(?), see also ParseTypeApplicationExpression   v
	' the idea is to look ahead to decide what to parse as relational and what to parse as a type
	' application, so that in case of an error, the result (error message and recovery) will be
	' as close as possible to the intended meaning
	Rem
	Method ParseRelationalCompatibleExpression:IRelationalCompatibleExpressionSyntaxData()
		Local lhs:IRelationalCompatibleExpressionSyntaxData = ParseUnionCompatibleExpression()
		If Not lhs Then Return Null
		'a<b>
		'a<b<c>
		
		' ideas:
		' - parse gen as postfix, in case of error abort only if gt is missing or there is an And or Or token inside
		' - parse relational, in case of lt scan ahead for gt taking care to balance parens - BAD (unbounded lookahead)
		' - 
		
		Local state:SParserState
		Local unmatchedLt:Int = 0
		Repeat
			Local opIsLt:Int = currentToken.Kind() = TTokenKind.Lt
			Local opIsGt:Int = currentToken.Kind() = TTokenKind.Gt
			If opIsLt Then
				If unmatchedLt = 0 Then
					' when encountering multiple lt, keep the state from before the first one
					' when encountering any other operator except gt, forget about the lt
					state = SaveState()
				End If
				unmatchedLt :+ 1
			Else If Not opIsGt
				unmatchedLt = 0
			End If
			
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.Eq])
			If Not op Then op = ParseOperator([TTokenKind.Neq])
			If Not op Then op = ParseOperator([TTokenKind.Lt])
			If Not op Then op = ParseOperator([TTokenKind.Gt])
			If Not op Then op = ParseOperator([TTokenKind.Leq])
			If Not op Then op = ParseOperator([TTokenKind.Geq])
			If op Then
				If opIsGt And unmatchedLt Then
					' type arguments can only appear after an identifier
					Local expressionBeforeLt:IExpressionSyntaxData = lhs
					For Local i:Int = 1 To pendingLt
						expressionBeforeLt = TRelationalExpressionSyntaxData(expressionBeforeLt).lhs
					Next
					If (TNameExpressionSyntaxData(expressionBeforeLt) Or TMemberAccessExpressionSyntaxData(expressionBeforeLt)) Then
						RestoreState state
						Local typeApplicationExpression:TTypeApplicationExpressionSyntaxData = ParseTypeApplicationExpression(IPostfixCompatibleExpressionSyntaxData(expressionBeforeLt))
						If typeApplicationExpression Then
							lhs = ParsePostfixCompatibleExpression(typeApplicationExpression)
							'state = SaveState()
							unmatchedLt = False
							Continue
						Else
							Throw "what now"
						End If
					End If
				End If
				
				Local rhs:IUnionCompatibleExpressionSyntaxData = ParseUnionCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TRelationalExpressionSyntaxData.Create(lhs, op, rhs)
				
				'previousOpWasLt = opIsLt
			Else
				Return lhs
			End If
		Forever
	End Method
	End Rem
	
	Method ParseUnionCompatibleExpression:IUnionCompatibleExpressionSyntaxData()
		Local lhs:IUnionCompatibleExpressionSyntaxData = ParseIntersectionCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.BitOr])
			If Not op Then op = ParseOperator([TTokenKind.BitNot])
			If op Then
				Local rhs:IIntersectionCompatibleExpressionSyntaxData = ParseIntersectionCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TUnionExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseIntersectionCompatibleExpression:IIntersectionCompatibleExpressionSyntaxData()
		Local lhs:IIntersectionCompatibleExpressionSyntaxData = ParseSumCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.BitAnd])
			If op Then
				Local rhs:ISumCompatibleExpressionSyntaxData = ParseSumCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TIntersectionExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseSumCompatibleExpression:ISumCompatibleExpressionSyntaxData()
		Local lhs:ISumCompatibleExpressionSyntaxData = ParseProductCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.Plus])
			If Not op Then op = ParseOperator([TTokenKind.Minus])
			If op Then
				Local rhs:IProductCompatibleExpressionSyntaxData = ParseProductCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TSumExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseProductCompatibleExpression:IProductCompatibleExpressionSyntaxData()
		Local lhs:IProductCompatibleExpressionSyntaxData = ParseExponentialCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.Mul])
			If Not op Then op = ParseOperator([TTokenKind.Div])
			If Not op Then op = ParseOperator([TTokenKind.Mod_])
			If Not op Then op = ParseOperator([TTokenKind.Shl_])
			If Not op Then op = ParseOperator([TTokenKind.Shr_])
			If Not op Then op = ParseOperator([TTokenKind.Sar_])
			If op Then
				Local rhs:IExponentialCompatibleExpressionSyntaxData = ParseExponentialCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TProductExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseExponentialCompatibleExpression:IExponentialCompatibleExpressionSyntaxData()
		Local lhs:IExponentialCompatibleExpressionSyntaxData = ParsePrefixCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntaxData
			If Not op Then op = ParseOperator([TTokenKind.Pow])
			If op Then
				Local rhs:IPrefixCompatibleExpressionSyntaxData = ParsePrefixCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = TExponentialExpressionSyntaxData.Create(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParsePrefixCompatibleExpression:IPrefixCompatibleExpressionSyntaxData()
		Local op:TOperatorSyntaxData
		If Not op Then op = ParseOperator([TTokenKind.Plus])
		If Not op Then op = ParseOperator([TTokenKind.Minus])
		If Not op Then op = ParseOperator([TTokenKind.BitNot])
		If Not op Then op = ParseOperator([TTokenKind.Not_])
		If Not op Then op = ParseOperator([TTokenKind.Asc_])
		If Not op Then op = ParseOperator([TTokenKind.Chr_])
		If Not op Then op = ParseOperator([TTokenKind.Len_])
		If Not op Then op = ParseOperator([TTokenKind.SizeOf_])
		If Not op Then op = ParseOperator([TTokenKind.Varptr_])
		If op Then
			Local arg:IPrefixCompatibleExpressionSyntaxData = ParsePrefixCompatibleExpression()
			If Not arg Then
				' TODO: is there a better way to handle this kind of error (skip the operator)?
				ReportError "Expected expression"
				arg = GenerateMissingExpression()
			End If
			
			Return TPrefixOperatorExpressionSyntaxData.Create(op, arg)
		Else
			Local castExpression:TTypeCastExpressionSyntaxData = ParseTypeCastExpression()
			If castExpression Then
				Return castExpression
			Else
				Return ParsePostfixCompatibleExpression(False)
			End If
		End If
	End Method
	
	Method ParseTypeCastExpression:TTypeCastExpressionSyntaxData()
		Local state:SParserState = SaveState()
		
		Local targetType:TTypeSyntaxData = ParseTypeCastType()
		If Not targetType Then Return Null
		If Not TKeywordTypeBaseSyntaxData(targetType.base) And Not targetType.suffixes Then
			' casts to types that begin with keywords (e.g.: Object x) are unambiguous,
			' and so are casts to types with a suffix (e.g.: T Ptr x), but casts to
			' non-keyword non-suffix types (e.g. T x) are syntactically indistinguishable from
			' call expressions, so they will be parsed as such
			' note: strictly speaking, this is only true when the type is followed by an open
			' parenthesis (in other words, when x is a paren expression) - otherwise a cast could
			' be distinguised from a call expression by the presence or absence of parentheses
			' and it could be distinguished from a paren-less call by the context it appears in
			' (the cast can only be an expression, the call can only be a statement)
			' but distinguishing between casts and calls like that would be complicated and not worth
			' the effort, so all non-keyword non-suffix type casts are just parsed as calls instead
			RestoreState state
			Return Null
		End If
		
		Local arg:IPrefixCompatibleExpressionSyntaxData = ParsePrefixCompatibleExpression()
		If Not arg Then
			ReportError "Expected expression"
			arg = GenerateMissingExpression()
		End If
		
		Return TTypeCastExpressionSyntaxData.Create(targetType, arg)
	End Method
	
	Method ParsePostfixCompatibleExpression:IPostfixCompatibleExpressionSyntaxData(parenlessCallStatement:Int)
		Local arg:IPostfixCompatibleExpressionSyntaxData = ParsePrimaryExpression()
		If Not arg Then arg = ParseMemberAccessExpression(Null) ' global scope access (leading dot)
		If Not arg Then Return Null
		
		Return ParsePostfixCompatibleExpression(arg, parenlessCallStatement)
	End Method
	
	Method ParsePostfixCompatibleExpression:IPostfixCompatibleExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData, parenlessCallStatement:Int)
		Local postfixExpression:IPostfixCompatibleExpressionSyntaxData
		postfixExpression = ParseMemberAccessExpression(arg);    If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression, parenlessCallStatement)
		postfixExpression = ParseIndexExpression(arg);           If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression, parenlessCallStatement)
		postfixExpression = ParseCallExpression(arg);            If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression, parenlessCallStatement)
		postfixExpression = ParseTypeApplicationExpression(arg, parenlessCallStatement); If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression, parenlessCallStatement)
		'postfixExpression = ParseTypeAssertionExpression(arg);   If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression)
		Return arg
	End Method
		
	Method ParseMemberAccessExpression:TMemberAccessExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData)
		Local dot:TSyntaxToken = TryTakeToken(TTokenKind.Dot)
		If Not dot Then Return Null
		
		Local memberName:TNameSyntaxData = ParseName()
		If Not memberName Then
			ReportError "Expected member name"
			memberName = GenerateMissingName()
		End If
		
		Return TMemberAccessExpressionSyntaxData.Create(arg, dot, memberName)
	End Method
	
	Method ParseIndexExpression:TIndexExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData)
		Local indexOperator:TBracketExpressionListSyntaxData = ParseBracketExpressionList(EEmptyElementsOption.Disallow)
		If Not indexOperator Then Return Null
		
		Return TIndexExpressionSyntaxData.Create(arg, indexOperator)
	End Method
	
	Method ParseCallExpression:TCallExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData)
		Local callOperator:TParenExpressionListSyntaxData = ParseParenExpressionList(EEmptyElementsOption.Allow)
		If Not callOperator Then Return Null
		
		Return TCallExpressionSyntaxData.Create(arg, callOperator)
	End Method
	
	' older implementation   v
	Rem
	Method ParseTypeApplicationExpression:TTypeApplicationExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData) ' backtracks
		If Not (TNameExpressionSyntaxData(arg) Or TMemberAccessExpressionSyntaxData(arg)) Then
			Return Null ' type arguments can only appear after an identifier
		End If
		
		Local state:SParserState = SaveState()
		
		Local errorsBefore:Int = parseErrors.Count()
		Local typeApplicationOperator:TTypeArgumentListSyntaxData = ParseTypeArgumentList()
		If Not typeApplicationOperator Then Return Null
		Local errorsAfter:Int = parseErrors.Count()
		If errorsBefore <> errorsAfter Then
			RestoreState state
			Return Null
		End If
		' TODO: if there are errors while parsing the type list, skip ahead to the gt;
		'       if the gt is found, examine the token after it as if it was successful
		'       this way, expressions like "a<b & c>(d)" will be parsed as generic
		'       problem: "a<b And c>(d)" and "a<b Or c>(d)" should not be parsed as a type list
		'       idea: if the if the type list parse fails, skip ahead to the gt, using And and Or
		'             as additional terminators; we do not need to be concerned about skipping
		'             lt tokens during this, because the goal is to consider anything that looks
		'             like a chain of comparisons as invalid
		'       problem: "a < b(x > (y + z))" should not be parsed as a generic
		
		' TODO: do not do this when parsing a call statement (with or without parentheses)
		Function CurrentTokenCanAppearAfterTypeArgumentList:Int(self_:TParser)
			Select self_.currentToken.Kind()
				' prefix operators, excluding the ones that double as binary operators
				Case TTokenKind.Not_, TTokenKind.Asc_, TTokenKind.Chr_, TTokenKind.Len_, TTokenKind.SizeOf_, TTokenKind.Varptr_
					Return False
				Case TTokenKind.LBracket
					???
				Case TTokenKind.LParen
					Return True
				Default
					'Local state:SParserState = self_.SaveState()
					' type keywords
					If self_.ParseKeywordTypeBase() Then
						'self_.RestoreState state
						Return False
					End If
					' starting tokens of primary expressions, excluding lparen
					If self_.ParsePrimaryExpression() Then
						'self_.RestoreState state
						Return False
					End If
					Return True
			End Select
		End Function
		If Not CurrentTokenCanAppearAfterTypeArgumentList(Self) Then
			RestoreState state
			Return Null
		End If
		
		Return TTypeApplicationExpressionSyntaxData.Create(arg, typeApplicationOperator)
	End Method
	End Rem
	Rem
	Method ParseTypeApplicationExpression:TTypeApplicationExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData) ' always succeeds
		Assert TNameExpressionSyntaxData(arg) Or TMemberAccessExpressionSyntaxData(arg) Else "???"
		
		Local typeApplicationOperator:TTypeArgumentListSyntaxData = ParseTypeArgumentList()
		
		Return TTypeApplicationExpressionSyntaxData.Create(arg, typeApplicationOperator)
	End Method
	End Rem
	Method ParseTypeApplicationExpression:TTypeApplicationExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData, parenlessCallStatement:Int) ' backtracks
		If Not CanBeFollowedByTypeApplication(arg) Then Return Null
		Function CanBeFollowedByTypeApplication:Int(arg:IExpressionSyntaxData)
			' type arguments can only be applied to names, not arbitrary expressions
			If TNameExpressionSyntaxData(arg) Then Return True
			If TMemberAccessExpressionSyntaxData(arg) Then Return True
			If TParenExpressionSyntaxData(arg) Then Return CanBeFollowedByTypeApplication(TParenExpressionSyntaxData(arg).expression)
			Return False
		End Function
		
		If currentToken.Kind() <> TTokenKind.Lt Then Return Null
		
		Local state:SParserState = SaveState()
		
		Local errorsBefore:Int = parseErrors.Count()
		Local typeApplicationOperator:TTypeArgumentListSyntaxData = ParseTypeArgumentList()
		If Not typeApplicationOperator Then Return Null
		Local errorsAfter:Int = parseErrors.Count()
		If errorsBefore <> errorsAfter Then
			' TODO: might be better to cancel only in the case of certain skipped tokens
			'       (e.g. "For x = a<b To c>d") or missing chevrons/parentheses/brackets
			'       (e.g. "a<b(c>d)")
			RestoreState state
			Return Null
		End If
		Rem
		Function HasMissingTokens:Int(s:ISyntaxData)
			For Local child:ISyntaxDataOrSyntaxToken = EachIn s.GetChildren()
				Local token:TSyntaxToken = TSyntaxToken(child)
				If token Then
					If token.lexerToken.missing Then
						Select token.Kind()
							Case TTokenKind.LParen, TTokenKind.RParen, TTokenKind.LBracket, TTokenKind.RBracket, TTokenKind.Lt, TTokenKind.Gt Return True
							' TODO: problem: "For x = a<b To c>d"
						End Select
					End If
				Else
					If HasMissingTokens(ISyntaxData(child)) Then Return True
				End If
			Next
			Return False
		End Function
		If HasMissingTokens(typeApplicationOperator) Then
			RestoreState state
			Return Null
		End If
		End Rem
		
		
		' TODO: if there are errors while parsing the type list, skip ahead to the gt;
		'       if the gt is found, examine the token after it as if it was successful
		'       this way, expressions like "a<b & c>(d)" will be parsed as generic
		'       problem: "a<b And c>(d)" and "a<b Or c>(d)" should not be parsed as a type list
		'       idea: if the if the type list parse fails, skip ahead to the gt, using And and Or
		'             as additional terminators; we do not need to be concerned about skipping
		'             lt tokens during this, because the goal is to consider anything that looks
		'             like a chain of comparisons as invalid
		'       problem: "a < b(x > (y + z))" should not be parsed as a generic
		
		
		If Not parenlessCallStatement Then
			' discard the parsed type application if the next token suggests that it
			' should be a chain of relational expressions instead;
			' this does NOT apply when trying to parse a paren-less call statement, because
			' the first argument for said call might be (almost) any expression,
			' e.g.: "a<b> Not c" is a valid paren-less call of a generic function, but in any
			' other context, the "Not" would make this two comparisons instead of a type application
			If Not CurrentTokenCanAppearAfterTypeArgumentList(Self) Then
				RestoreState state
				Return Null
			End If
			Function CurrentTokenCanAppearAfterTypeArgumentList:Int(self_:TParser) ' (in the same statement)
				' this rejects all tokens that can be the start of an expression appearing on the rhs of
				' a relational expression but not appear directly after a type argument list in valid code
				Select self_.currentToken.Kind()
					' prefix operators, excluding the ones that double as binary operators
					Case TTokenKind.Not_, TTokenKind.Asc_, TTokenKind.Chr_, TTokenKind.Len_, TTokenKind.SizeOf_, TTokenKind.Varptr_
						Return False
					' open bracket of array literal or indexing (cannot be a type suffix in this context)
					Case TTokenKind.LBracket ' avoids expensive path in ParsePrimaryExpression in Default case below
						Return False
					' open parenthesis of call (takes priority over paren expression after a possible type argument list)
					Case TTokenKind.LParen ' avoids expensive path in ParsePrimaryExpression in Default case below
						Return True
					' New keyword of New expression
					Case TTokenKind.New_ ' avoids expensive path in ParsePrimaryExpression in Default case below
						Return False
					Default
						' type keyword of any type with a keyword base
						If self_.ParseKeywordTypeBase() Then Return False
						' any primary expression, excluding paren expression
						If self_.ParsePrimaryExpression() Then Return False
						' anything else
						Return True
				End Select
			End Function
		End If
		
		Return TTypeApplicationExpressionSyntaxData.Create(arg, typeApplicationOperator)
	End Method
	
	Method ParseTypeAssertionExpression:TTypeAssertionExpressionSyntaxData(arg:IPostfixCompatibleExpressionSyntaxData)
		Throw "TODO"
	End Method
	
	Method ParsePrimaryExpression:IPrimaryExpressionSyntaxData()
		' if any of these check more than just the current token, adjust the check in
		' CurrentTokenCanAppearAfterTypeArgumentList in ParseTypeApplicationExpression
		Local primaryExpression:IPrimaryExpressionSyntaxData
		primaryExpression = ParseParenExpression();   If primaryExpression Then Return primaryExpression
		primaryExpression = ParseNewExpression();     If primaryExpression Then Return primaryExpression
		primaryExpression = ParseSelfExpression();    If primaryExpression Then Return primaryExpression
		primaryExpression = ParseSuperExpression();   If primaryExpression Then Return primaryExpression
		primaryExpression = ParseNullExpression();    If primaryExpression Then Return primaryExpression
		primaryExpression = ParseTrueExpression();    If primaryExpression Then Return primaryExpression
		primaryExpression = ParseFalseExpression();   If primaryExpression Then Return primaryExpression
		primaryExpression = ParsePiExpression();      If primaryExpression Then Return primaryExpression
		'primaryExpression = ParseMinMaxExpression();  If primaryExpression Then Return primaryExpression
		primaryExpression = ParseNameExpression();    If primaryExpression Then Return primaryExpression
		primaryExpression = ParseLiteralExpression(); If primaryExpression Then Return primaryExpression
		Return Null
	End Method
	
	Method ParseParenExpression:TParenExpressionSyntaxData()
		Local lparen:TSyntaxToken = TryTakeToken(TTokenKind.LParen)
		If Not lparen Then Return Null
		
		Local innerExpression:IExpressionSyntaxData = ParseExpression()
		If Not innerExpression Then
			ReportError "Expected expression"
			innerExpression = GenerateMissingExpression()
		End If
		
		Local rparen:TSyntaxToken = TakeToken(TTokenKind.RParen)
		
		Return TParenExpressionSyntaxData.Create(lparen, innerExpression, rparen)
	End Method
	
	Method ParseNewExpression:TNewExpressionSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.New_)
		If Not keyword Then Return Null
		
		Local type_:TTypeSyntaxData = ParseNewConstructibleType()
		If Not type_ Then
			ReportError "Expected type"
			type_ = GenerateMissingType()
		End If
		
		Local callOperator:TParenExpressionListSyntaxData = ParseParenExpressionList(EEmptyElementsOption.Allow)
				
		Return TNewExpressionSyntaxData.Create(keyword, type_, callOperator)
	End Method
	
	Method ParseSelfExpression:TSelfExpressionSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Self_)
		If Not token Then Return Null
		
		Return TSelfExpressionSyntaxData.Create(token)
	End Method
	
	Method ParseSuperExpression:TSuperExpressionSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Super_)
		If Not token Then Return Null
		
		Return TSuperExpressionSyntaxData.Create(token)
	End Method
	
	Method ParseNullExpression:TNullExpressionSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Null_)
		If Not token Then Return Null
		
		Return TNullExpressionSyntaxData.Create(token)
	End Method
	
	Method ParseTrueExpression:TTrueExpressionSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.True_)
		If Not token Then Return Null
		
		Return TTrueExpressionSyntaxData.Create(token)
	End Method
	
	Method ParseFalseExpression:TFalseExpressionSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.False_)
		If Not token Then Return Null
		
		Return TFalseExpressionSyntaxData.Create(token)
	End Method
	
	Method ParsePiExpression:TPiExpressionSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Pi_)
		If Not token Then Return Null
		
		Return TPiExpressionSyntaxData.Create(token)
	End Method
	
	Method ParseNameExpression:TNameExpressionSyntaxData()
		Local name:TNameSyntaxData = ParseName()
		If Not name Then Return Null
		
		Return TNameExpressionSyntaxData.Create(name)
	End Method
	
	Method ParseLiteralExpression:TLiteralExpressionSyntaxData()
		' if any of these check more than just the current token, adjust the check in
		' CurrentTokenCanAppearAfterTypeArgumentList in ParseTypeApplicationExpression
		Local literalExpression:TLiteralExpressionSyntaxData
		literalExpression = ParseArrayLiteralExpression();   If literalExpression Then Return literalExpression
		literalExpression = ParseNumericLiteralExpression(); If literalExpression Then Return literalExpression
		literalExpression = ParseStringLiteralExpression();  If literalExpression Then Return literalExpression
		Return Null
	End Method
	
	Method ParseNumericLiteralExpression:TNumericLiteralExpressionSyntaxData()
		Local value:TSyntaxToken = TryTakeToken([TTokenKind.IntLiteral, TTokenKind.HexIntLiteral, TTokenKind.BinIntLiteral, TTokenKind.FloatLiteral])
		If Not value Then Return Null
		
		Local type_:TTypeSyntaxData = ParseLiteralExpressionType()
		
		Return TNumericLiteralExpressionSyntaxData.Create(value, type_)
	End Method
	
	Method ParseStringLiteralExpression:TStringLiteralExpressionSyntaxData()
		Local value:TSyntaxToken = TryTakeToken(TTokenKind.StringLiteral)
		If Not value Then Return Null
		
		Local type_:TTypeSyntaxData = ParseLiteralExpressionType()
		
		Return TStringLiteralExpressionSyntaxData.Create(value, type_)
	End Method
	
	Method ParseArrayLiteralExpression:TArrayLiteralExpressionSyntaxData()
		Local elementList:TBracketExpressionListSyntaxData = ParseBracketExpressionList(EEmptyElementsOption.Disallow)
		If Not elementList Then Return Null
		
		Return TArrayLiteralExpressionSyntaxData.Create(elementList)
	End Method
	
	Method ParseConstantExpression:IExpressionSyntaxData()
		Return ParseExpression()
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Types ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseType:TTypeSyntaxData(typeParseMode:ETypeParseMode, colonTypeMode:EColonTypeMode, callableTypeOption:ECallableTypeOption, arrayDimensionsOption:EArrayDimensionsOption) ' backtracks
		' TODO: improve code, hard to understand
		Assert Not (colonTypeMode = EColonTypeMode.Colon And typeParseMode = ETypeParseMode.Noncommittal) Else "Cannot combine Colon and Noncommittal"
		
		Local state:SParserState
		If typeParseMode = ETypeParseMode.Noncommittal Then state = SaveState()
		
		Local colon:TSyntaxToken
		If colonTypeMode = EColonTypeMode.Colon Then
			colon = TryTakeToken(TTokenKind.Colon)
		End If
		
		Local allowSigilTypeBase:Int = colonTypeMode = EColonTypeMode.Colon
		Local base:TTypeBaseSyntaxData
		If colonTypeMode = EColonTypeMode.Colon And Not colon Then
			base = ParseSigilTypeBase()
		Else
			base = ParseTypeBase()
		End If
		
		If Not colon And Not base Then
			If callableTypeOption = ECallableTypeOption.Allow And currentToken.Kind() = TTokenKind.LParen Then
				base = Null	' void-returning callable type, no issues
			Else
				Return Null ' there does not seem to be a type here
			End If
		Else If colon And Not base Then
			ReportError "Expected type" ' base stays null ' TODO: better message?
		Else If TSigilTypeBaseSyntaxData(base) And Not allowSigilTypeBase Then
			If typeParseMode = ETypeParseMode.Committal Then
				ReportError "Expected non-sigil type" ' base stays as-is ' TODO: better message?
			Else
				RestoreState state
				Return Null
			End If
		End If
		
		Local marshallingModifier:TTypeMarshallingModifierSyntaxData
		If TSigilTypeBaseSyntaxData(base) And TSigilTypeBaseSyntaxData(base).sigil.Kind() = TTokenKind.StringSigil Then
			marshallingModifier = ParseTypeMarshallingModifier()
		End If
		
		Local typeArgumentList:TTypeArgumentListSyntaxData
		If TQualifiedNameTypeBaseSyntaxData(base) Then
			If typeParseMode = ETypeParseMode.Noncommittal Then
				If currentToken.Kind() = TTokenKind.Lt Then
					Local state:SParserState = SaveState()
					Local errorsBefore:Int = parseErrors.Count()
					typeArgumentList = ParseTypeArgumentList()
					Local errorsAfter:Int = parseErrors.Count()
					If errorsBefore <> errorsAfter Then
						RestoreState state
						typeArgumentList = Null
					End If
				End If
			Else
				typeArgumentList = ParseTypeArgumentList()
			End If
		End If
		
		Local suffixes:TTypeSuffixSyntaxData[]
		Assert Not (typeParseMode = ETypeParseMode.Noncommittal And callableTypeOption = ECallableTypeOption.Allow) Else "Cannot combine ECallableTypeOption.Allow and Noncommittal"
		If currentToken.Kind() <> TTokenKind.Lt Then
			Repeat
				Local stateBeforeSuffix:SParserState
				If typeParseMode = ETypeParseMode.Noncommittal Then stateBeforeSuffix = SaveState()
				Local suffix:TTypeSuffixSyntaxData = ParseTypeSuffix(callableTypeOption, arrayDimensionsOption)
				If suffix Then
					If HasAnyArrayDimensions(suffix) Then
						If arrayDimensionsOption = EArrayDimensionsOption.Disallow Then
							If typeParseMode = ETypeParseMode.Noncommittal Then
								' this suffix should not have been parsed, is probably an index operator instead
								RestoreState stateBeforeSuffix
								Exit
							End If
						Else
							' if dimensions have been specified, it must have been the last suffix
							suffixes :+ [suffix]
							Exit
						End If
					End If
					suffixes :+ [suffix]
				Else
					Exit
				End If
			Forever
		End If
		
		Return TTypeSyntaxData.Create(colon, base, marshallingModifier, typeArgumentList, suffixes)
	End Method
	
	Method ParseSuperType:TTypeSyntaxData()
		Return ParseType(ETypeParseMode.Committal, EColonTypeMode.NoColon, ECallableTypeOption.Disallow, EArrayDimensionsOption.Disallow)
	End Method
	
	Method ParseSuperTypeList:TTypeListSyntaxData()
		Return ParseTypeList(ETypeParseMode.Committal, EColonTypeMode.NoColon, ECallableTypeOption.Disallow, EArrayDimensionsOption.Disallow)
	End Method
	
	Method ParseEnumBaseType:TTypeSyntaxData()
		Return ParseType(ETypeParseMode.Committal, EColonTypeMode.Colon, ECallableTypeOption.Disallow, EArrayDimensionsOption.Disallow)
	End Method
	
	Method ParseVariableDeclaratorType:TTypeSyntaxData(arrayDimensionsOption:EArrayDimensionsOption)
		Return ParseType(ETypeParseMode.Committal, EColonTypeMode.Colon, ECallableTypeOption.Allow, arrayDimensionsOption)
	End Method
	
	Method ParseTypeCastType:TTypeSyntaxData()
		Return ParseType(ETypeParseMode.Noncommittal, EColonTypeMode.NoColon, ECallableTypeOption.Disallow, EArrayDimensionsOption.Disallow)
	End Method
	
	Method ParseNewConstructibleType:TTypeSyntaxData()
		Return ParseType(ETypeParseMode.Committal, EColonTypeMode.NoColon, ECallableTypeOption.Disallow, EArrayDimensionsOption.Require)
	End Method
	
	Method ParseLiteralExpressionType:TTypeSyntaxData()
		Return ParseType(ETypeParseMode.Committal, EColonTypeMode.Colon, ECallableTypeOption.Disallow, EArrayDimensionsOption.Disallow)
	End Method
	
	Method ParseTypeBase:TTypeBaseSyntaxData()
		Local typeBase:TTypeBaseSyntaxData
		typeBase = ParseQualifiedNameTypeBase(); If typeBase Then Return typeBase
		typeBase = ParseKeywordTypeBase();       If typeBase Then Return typeBase
		typeBase = ParseSigilTypeBase();         If typeBase Then Return typeBase
		Return Null
	End Method
	
	Method ParseKeywordTypeBase:TKeywordTypeBaseSyntaxData()
		Local keyword:TSyntaxToken = TryTakeToken([ ..
			TTokenKind.Byte_, ..
			TTokenKind.Short_, ..
			TTokenKind.Int_, ..
			TTokenKind.UInt_, ..
			TTokenKind.Long_, ..
			TTokenKind.ULong_, ..
			TTokenKind.Size_T_, ..
			TTokenKind.Float_, ..
			TTokenKind.Double_, ..
			TTokenKind.String_, ..
			TTokenKind.Object_, ..
			TTokenKind.LParam_, ..
			TTokenKind.WParam_, ..
			TTokenKind.Float64_, ..
			TTokenKind.Float128_, ..
			TTokenKind.Double128_, ..
			TTokenKind.Int128_ ..
		])
		If Not keyword Then Return Null
		
		Return TKeywordTypeBaseSyntaxData.Create(keyword)
	End Method
	
	Method ParseSigilTypeBase:TSigilTypeBaseSyntaxData()
		Local sigil:TSyntaxToken = TryTakeToken([ ..
			TTokenKind.ByteSigil, ..
			TTokenKind.IntSigil, ..
			TTokenKind.FloatSigil, ..
			TTokenKind.DoubleSigil, ..
			TTokenKind.StringSigil ..
		])
		If Not sigil Then Return Null
		
		Return TSigilTypeBaseSyntaxData.Create(sigil)
	End Method
	
	Method ParseQualifiedNameTypeBase:TQualifiedNameTypeBaseSyntaxData()
		Local name:TQualifiedNameSyntaxData = ParseQualifiedName()
		If Not name Then Return Null
		
		Return TQualifiedNameTypeBaseSyntaxData.Create(name)
	End Method
	
	Method ParseTypeMarshallingModifier:TTypeMarshallingModifierSyntaxData()
		Local modifier:TTypeMarshallingModifierSyntaxData
		modifier = ParseCStringTypeMarshallingModifier(); If modifier Then Return modifier
		modifier = ParseWStringTypeMarshallingModifier(); If modifier Then Return modifier
		Return Null
	End Method
	
	Method ParseCStringTypeMarshallingModifier:TCStringTypeMarshallingModifierSyntaxData()
		Local keyword:TContextualKeywordSyntaxData = ParseContextualKeyword("z")
		If keyword Then Return TCStringTypeMarshallingModifierSyntaxData.Create(keyword)
		Return Null
	End Method
	
	Method ParseWStringTypeMarshallingModifier:TWStringTypeMarshallingModifierSyntaxData()
		Local keyword:TContextualKeywordSyntaxData = ParseContextualKeyword("w")
		If keyword Then Return TWStringTypeMarshallingModifierSyntaxData.Create(keyword)
		Return Null
	End Method
	
	Method ParseTypeSuffix:TTypeSuffixSyntaxData(callableTypeOption:ECallableTypeOption, arrayDimensionsOption:EArrayDimensionsOption)		
		Local typeSuffix:TTypeSuffixSyntaxData
		If callableTypeOption = ECallableTypeOption.Allow Then
			typeSuffix = ParseCallableTypeSuffix(); If typeSuffix Then Return typeSuffix
		End If
		typeSuffix = ParseArrayTypeSuffix(arrayDimensionsOption); If typeSuffix Then Return typeSuffix
		typeSuffix = ParsePtrTypeSuffix(); If typeSuffix Then Return typeSuffix
		typeSuffix = ParseVarTypeSuffix(); If typeSuffix Then Return typeSuffix
		Return Null
	End Method
	
	Method ParsePtrTypeSuffix:TPtrTypeSuffixSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Ptr_)
		If Not token Then Return Null
		
		Return TPtrTypeSuffixSyntaxData.Create(token)
	End Method
	
	Method ParseVarTypeSuffix:TVarTypeSuffixSyntaxData()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Var_)
		If Not token Then Return Null
		
		Return TVarTypeSuffixSyntaxData.Create(token)
	End Method
	
	Method ParseArrayTypeSuffix:TArrayTypeSuffixSyntaxData(arrayDimensionsOption:EArrayDimensionsOption)
		' will not fail if arrayDimensionsOption is violated; only report errors
		Local lbracket:TSyntaxToken = TryTakeToken(TTokenKind.LBracket)
		If Not lbracket Then Return Null
		
		Local emptyElementsOption:EEmptyElementsOption
		Select arrayDimensionsOption ' TODO: report error if some dimensions are specified but some are not
			Case EArrayDimensionsOption.Disallow emptyElementsOption = EEmptyElementsOption.Require
			Case EArrayDimensionsOption.Allow    emptyElementsOption = EEmptyElementsOption.Allow
			Case EArrayDimensionsOption.Require  emptyElementsOption = EEmptyElementsOption.Disallow
			Default RuntimeError "Missing case"
		End Select
		Local dimensionsList:TExpressionListSyntaxData = ParseExpressionList(emptyElementsOption)
		
		Local rbracket:TSyntaxToken = TakeToken(TTokenKind.RBracket)
		
		Return TArrayTypeSuffixSyntaxData.Create(lbracket, dimensionsList, rbracket)
	End Method
	
	Method ParseCallableTypeSuffix:TCallableTypeSuffixSyntaxData()
		Local lparen:TSyntaxToken = TryTakeToken(TTokenKind.LParen)
		If Not lparen Then Return Null
		
		Local parameterDeclaration:TVariableDeclarationSyntaxData = ParseParameterVariableDeclaration()
		
		Local rparen:TSyntaxToken = TakeToken(TTokenKind.RParen)
		
		Return TCallableTypeSuffixSyntaxData.Create(lparen, parameterDeclaration, rparen)
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Auxiliary Constructs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseVariableDeclaratorList:TVariableDeclaratorListSyntaxData(initializersOption:EInitializersOption) ' always succeeds
		Local elements:TVariableDeclaratorListElementSyntaxData[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local declarator:TVariableDeclaratorSyntaxData = ParseVariableDeclarator(initializersOption)
			If Not declarator Then
				If Not elements Then
					Exit
				Else
					ReportError "Expected variable declarator"
				End If
			End If
			elements :+ [TVariableDeclaratorListElementSyntaxData.Create(comma, declarator)]
		Forever
		Return TVariableDeclaratorListSyntaxData.Create(elements)
	End Method
	
	Method ParseTypeParameterDeclaratorList:TTypeParameterDeclaratorListSyntaxData() ' always succeeds
		Local elements:TTypeParameterDeclaratorListElementSyntaxData[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local declarator:TTypeParameterDeclaratorSyntaxData = ParseTypeParameterDeclarator()
			If Not declarator Then
				If Not elements Then
					Exit
				Else
					ReportError "Expected type variable declarator"
				End If
			End If
			elements :+ [TTypeParameterDeclaratorListElementSyntaxData.Create(comma, declarator)]
		Forever
		Return TTypeParameterDeclaratorListSyntaxData.Create(elements)
	End Method
	
	Method ParseParenExpressionList:TParenExpressionListSyntaxData(emptyElementsOption:EEmptyElementsOption)
		Local lparen:TSyntaxToken = TryTakeToken(TTokenKind.LParen)
		If Not lparen Then Return Null
		
		Local list:TExpressionListSyntaxData = ParseExpressionList(emptyElementsOption)
		
		Local rparen:TSyntaxToken = TakeToken(TTokenKind.RParen)
		
		Return TParenExpressionListSyntaxData.Create(lparen, list, rparen)
	End Method
	
	Method ParseBracketExpressionList:TBracketExpressionListSyntaxData(emptyElementsOption:EEmptyElementsOption)
		Local lbracket:TSyntaxToken = TryTakeToken(TTokenKind.LBracket)
		If Not lbracket Then Return Null
		
		Local list:TExpressionListSyntaxData = ParseExpressionList(emptyElementsOption)
		
		Local rbracket:TSyntaxToken = TakeToken(TTokenKind.RBracket)
		
		Return TBracketExpressionListSyntaxData.Create(lbracket, list, rbracket)
	End Method
	
	Method ParseExpressionList:TExpressionListSyntaxData(emptyElementsOption:EEmptyElementsOption) ' always succeeds
		Local elements:TExpressionListElementSyntaxData[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local expression:IExpressionSyntaxData = ParseExpression()
			If expression Then
				If emptyElementsOption = EEmptyElementsOption.Require Then
					ReportError "Expected ," ' TODO: skip tokens
				End If
			Else
				If Not elements And currentToken.Kind() <> TTokenKind.Comma Then
					Exit ' empty list
				Else If emptyElementsOption = EEmptyElementsOption.Disallow Then
					ReportError "Expected expression"
				End If
			End If
			elements :+ [TExpressionListElementSyntaxData.Create(comma, expression)]
		Forever
		Return TExpressionListSyntaxData.Create(elements)
	End Method
	
	Method ParseTypeParameterList:TTypeParameterListSyntaxData()
		Local lchevron:TSyntaxToken = TryTakeToken(TTokenKind.Lt)
		If Not lchevron Then Return Null
		
		Local list:TTypeParameterDeclaratorListSyntaxData = ParseTypeParameterDeclaratorList()
		
		Local rchevron:TSyntaxToken = TakeToken(TTokenKind.Gt)
		
		Return TTypeParameterListSyntaxData.Create(lchevron, list, rchevron)
	End Method
	
	Method ParseTypeArgumentList:TTypeArgumentListSyntaxData()
		Local lchevron:TSyntaxToken = TryTakeToken(TTokenKind.Lt)
		If Not lchevron Then Return Null
		
		Local list:TTypeListSyntaxData = ParseTypeList(ETypeParseMode.Committal, EColonTypeMode.NoColon, ECallableTypeOption.Allow, EArrayDimensionsOption.Disallow)
		
		Local rchevron:TSyntaxToken = TakeToken(TTokenKind.Gt)
		
		Return TTypeArgumentListSyntaxData.Create(lchevron, list, rchevron)
	End Method
	
	Method ParseTypeList:TTypeListSyntaxData(typeParseMode:ETypeParseMode, colonTypeMode:EColonTypeMode, callableTypeOption:ECallableTypeOption, arrayDimensionsOption:EArrayDimensionsOption) ' always succeeds
		Local elements:TTypeListElementSyntaxData[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma) ' TODO: put comma on terminator stack?
				If Not comma Then Exit
			End If
			Local type_:TTypeSyntaxData = ParseType(typeParseMode, colonTypeMode, callableTypeOption, arrayDimensionsOption)
			If Not type_ Then
				ReportError "Expected type"
			End If
			elements :+ [TTypeListElementSyntaxData.Create(comma, type_)]
		Forever
		Return TTypeListSyntaxData.Create(elements)
	End Method
	
	Method ParseQualifiedNameList:TQualifiedNameListSyntaxData() ' always succeeds
		Local elements:TQualifiedNameListElementSyntaxData[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local name:TQualifiedNameSyntaxData = ParseQualifiedName()
			If Not name Then
				ReportError "Expected identifier"
			End If
			elements :+ [TQualifiedNameListElementSyntaxData.Create(comma, name)]
		Forever
		Return TQualifiedNameListSyntaxData.Create(elements)
	End Method
	
	Method ParseMetaData:TMetaDataSyntaxData()
		Local lbrace:TSyntaxToken = TryTakeToken(TTokenKind.LBrace)
		If Not lbrace Then Return Null
		
		Local elements:TMetaDataElementSyntaxData[]
		Repeat
			Local key:TNameSyntaxData = ParseName()
			If Not key Then Exit
			Local eq:TSyntaxToken = TryTakeToken(TTokenKind.Eq)
			Local value:IExpressionSyntaxData
			If eq Then
				value = ParseConstantExpression()
				If Not value Then
					ReportError "Expected constant value"
					value = TStringLiteralExpressionSyntaxData.Create(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
				End If
			End If
			elements :+ [TMetaDataElementSyntaxData.Create(key, eq, value)]
		Forever
		
		Local rbrace:TSyntaxToken = TakeToken(TTokenKind.RBrace)
		
		Return TMetaDataSyntaxData.Create(lbrace, elements, rbrace)
	End Method
	
	Method ParseName:TNameSyntaxData()
		Local identifier:TSyntaxToken = TryTakeToken(TTokenKind.Identifier)
		If Not identifier Then Return Null
		
		Return TNameSyntaxData.Create(identifier)
	End Method
	
	Method ParseQualifiedName:TQualifiedNameSyntaxData()
		Local parts:TQualifiedNamePartSyntaxData[]
		If True Then ' first part
			Local dot:TSyntaxToken = TryTakeToken(TTokenKind.Dot)
			If dot Then ' identifier with leading dot
				parts = [TQualifiedNamePartSyntaxData.Create(dot, Null)]
			Else
				Local identifier:TSyntaxToken = TryTakeToken(TTokenKind.Identifier)
				If identifier Then
					parts = [TQualifiedNamePartSyntaxData.Create(Null, identifier)]
				Else
					Return Null
				End If
			End If
		End If
		Repeat ' other parts
			Local dot:TSyntaxToken = TryTakeToken(TTokenKind.Dot)
			If Not dot Then Exit
			Local identifier:TSyntaxToken = TryTakeToken(TTokenKind.Identifier)
			If Not identifier Then
				ReportError "Expected identifier"
				identifier = GenerateMissingIdentifier()
			End If
			parts :+ [TQualifiedNamePartSyntaxData.Create(dot, identifier)]
		Forever
		
		Return TQualifiedNameSyntaxData.Create(parts)
	End Method
	
	Method ParseOperator:TOperatorSyntaxData(tokenKinds:TTokenKind[]) ' backtracks
		' backtracks only when more than one token kind is passed
		Assert tokenKinds Else "Operator must consist of at least one token"
		
		Local state:SParserState
		If tokenKinds.length > 1 Then state = SaveState() ' TODO: performance
		
		Local tokens:TSyntaxToken[tokenKinds.length]
		For Local t:Int = 0 Until tokenKinds.length
			tokens[t] = TryTakeToken(tokenKinds[t])
			If Not tokens[t] Then
				If t > 0 Then RestoreState state
				Return Null
			End If
			If t > 0 Then
				' do not allow any trivia inbetween the tokens that make up the operator
				If tokens[t - 1].trailingTrivia Or tokens[t].leadingTrivia Then
					RestoreState state
					Return Null
				End If
			End If
		Next
		
		Return TOperatorSyntaxData.Create(tokens)
	End Method
	
	Method ParseContextualKeyword:TContextualKeywordSyntaxData(canonicalValue:String)
		' parses an identifier that has canonicalValue as its value (by case-insensitive comparison)
		If currentToken.Kind() = TTokenKind.Identifier And currentToken.lexerToken.value.ToLower() = canonicalValue.ToLower() Then
			Local identifier:TSyntaxToken = TakeToken(TTokenKind.Identifier)
			Return TContextualKeywordSyntaxData.Create(canonicalValue, identifier)
		Else
			Return Null
		End If
	End Method
	
	Method ParseAssignment:TAssignmentSyntaxData(assignmentMode:EAssignmentMode)
		Local op:TOperatorSyntaxData
		Select assignmentMode
			Case EAssignmentMode.Regular, EAssignmentMode.Constant
				op = ParseOperator([TTokenKind.Eq])
			Case EAssignmentMode.RegularOrCompound
				op = ParseOperator([TTokenKind.Eq])
				If Not op Then op = ParseOperator([TTokenKind.ColonPlus])
				If Not op Then op = ParseOperator([TTokenKind.ColonMinus])
				If Not op Then op = ParseOperator([TTokenKind.ColonMul])
				If Not op Then op = ParseOperator([TTokenKind.ColonDiv])
				If Not op Then op = ParseOperator([TTokenKind.ColonMod])
				If Not op Then op = ParseOperator([TTokenKind.ColonBitAnd])
				If Not op Then op = ParseOperator([TTokenKind.ColonBitOr])
				If Not op Then op = ParseOperator([TTokenKind.ColonBitNot])
				If Not op Then op = ParseOperator([TTokenKind.ColonShl])
				If Not op Then op = ParseOperator([TTokenKind.ColonShr])
				If Not op Then op = ParseOperator([TTokenKind.ColonSar])
			Default RuntimeError "Missing case"
		End Select
		If Not op Then Return Null
		
		Local expression:IExpressionSyntaxData
		If assignmentMode = EAssignmentMode.Constant Then
			expression = ParseConstantExpression()
			If Not expression Then
				ReportError "Expected constant expression"
				expression = GenerateMissingExpression()
			End If
		Else
			expression = ParseExpression()
			If Not expression Then
				ReportError "Expected expression"
				expression = GenerateMissingExpression()
			End If
		End If
		
		Return TAssignmentSyntaxData.Create(op, expression)
	End Method
	
	Method ParseExternSignatureAssignment:TExternSignatureAssignmentSyntaxData()
		Local op:TOperatorSyntaxData = ParseOperator([TTokenKind.Eq])
		If Not op Then Return Null
		
		Local externSignature:TSyntaxToken = TakeToken(TTokenKind.StringLiteral)
		
		Return TExternSignatureAssignmentSyntaxData.Create(op, externSignature)
	End Method
	
	Method ParseStatementSeparator:TStatementSeparatorSyntaxData() ' parses a logical newline (semicolon or non-escaped line break)
		Local token:TSyntaxToken = TryTakeToken(StatementSeparatorTokenKinds)
		If Not token Then Return Null
		
		Return TStatementSeparatorSyntaxData.Create(token)
	End Method
	
	Method ParseStatementSeparators:TStatementSeparatorSyntaxData[]()
		Local separators:TStatementSeparatorSyntaxData[]
		Repeat
			Local separator:TStatementSeparatorSyntaxData = ParseStatementSeparator()
			If separator Then separators :+ [separator] Else Exit
		Forever
		Return separators
	End Method
	
	Function HasAnyArrayDimensions:Int(suffix:TTypeSuffixSyntaxData) ' returns True if there is at least one
		Local arraySuffix:TArrayTypeSuffixSyntaxData = TArrayTypeSuffixSyntaxData(suffix)
		If arraySuffix Then
			For Local element:TExpressionListElementSyntaxData = EachIn arraySuffix.dimensionsList.elements
				If element.expression Then Return True
			Next
		End If
		Return False
	End Function
	
End Type
