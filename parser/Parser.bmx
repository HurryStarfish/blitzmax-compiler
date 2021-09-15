SuperStrict
Import "IParser.bmx"
Import "../lexer/ILexer.bmx"
Import "../syntax/Syntax.bmx"



Private

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



Struct SParserState
	Private
	Field ReadOnly lexerPosition:ILexerPosition
	Field ReadOnly currentToken:TSyntaxToken
	Field ReadOnly nextLexerToken:TLexerToken
	Field ReadOnly nextLeadingTrivia:TLexerToken[]
	' TODO: include terminator stack and error list?
	
	Public
	Method New(lexerPosition:ILexerPosition, currentToken:TSyntaxToken, nextLexerToken:TLexerToken, nextLeadingTrivia:TLexerToken[])
		Self.lexerPosition = lexerPosition
		Self.currentToken = currentToken
		Self.nextLexerToken = nextLexerToken
		Self.nextLeadingTrivia = nextLeadingTrivia
	End Method
End Struct

Type TTerminatorStack Final
	Private
	Field stack:TTokenKind[][8]
	Field stackSize:Int = 0
	
	Public
	Method Push(possibleTerminatorKinds:TTokenKind[]) ' multiple options are possible
		Assert possibleTerminatorKinds Else "Must specify at least one terminator"
		If stack.length = stackSize Then stack = stack[..stack.length * 2]
		stack[stackSize] = possibleTerminatorKinds
		stackSize :+ 1
	End Method
	
	Method Pop()
		Assert stackSize > 0 Else "Cannot pop terminator from empty stack"
		stack[StackSize - 1] = Null
		stackSize :- 1
	End Method
	
	Method Contains:Int(terminatorKind:TTokenKind)
		For Local kinds:TTokenKind[] = EachIn stack
			For Local kind:TTokenKind = EachIn kinds
				If kind = terminatorKind Then Return True
			Next
		Next
		Return False
	End Method
End Type



Public

Type TParser Implements IParser
	
	Field ReadOnly lexer:ILexer
	Field currentToken:TSyntaxToken
	Field parseErrors:TList = New TList'<TParseError>
		
	Private
	Field nextLexerToken:TLexerToken
	Field nextLeadingTrivia:TLexerToken[]
	
	Field terminatorStack:TTerminatorStack = New TTerminatorStack
	
	Method New() End Method
	
	Public
	Method New(lexer:ILexer)
		Self.lexer = lexer
		AdvanceToNextSyntaxToken()
	End Method
	
	Method Errors:TList() Override '<TParseError>
		Return parseErrors
	End Method
	
	Function IsDefiniteTrivia:Int(token:TLexerToken)
		Return ..
			token.kind = TTokenKind.Whitespace Or ..
			token.kind = TTokenKind.Comment Or ..
			token.kind = TTokenKind.Rem_ Or ..
			token.kind = TTokenKind.RemComment Or ..
			token.kind = TTokenKind.EndRem_
	End Function
	
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
		
		Local atLineStart:Int = Not currentToken Or currentToken.kind = TTokenKind.Linebreak
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
		currentToken = New TSyntaxToken(newCurrentLexerToken, newCurrentLeadingTrivia, newCurrentTrailingTrivia)
	End Method
	
	Method SaveState:SParserState()
		Local lexerPosition:ILexerPosition = lexer.GetPosition()
		Return New SParserState(lexerPosition, currentToken, nextLexerToken, nextLeadingTrivia)
	End Method
	
	Method RestoreState(pos:SParserState)
		lexer.SetPosition pos.lexerPosition
		currentToken = pos.currentToken
		nextLexerToken = pos.nextLexerToken
		nextLeadingTrivia = pos.nextLeadingTrivia
	End Method
	
	Public
	
	Method TakeToken:TSyntaxToken()
		' takes the current token
		Local token:TSyntaxToken = currentToken
		AdvanceToNextSyntaxToken()
		Return token
	End Method
	
	Method TakeToken:TSyntaxToken(kind:TTokenKind)
		' takes a token of the specified kind, reporting an error if the current token does not match
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
	
	Method GenerateMissingToken:TSyntaxToken(kind:TTokenKind)
		Return GenerateMissingToken(kind, kind.ToCode())
	End Method
	
	Method GenerateMissingToken:TSyntaxToken(kind:TTokenKind, value:String)
		Local missingTokenLocation:SCodeLocation = currentToken.lexerToken.location
		Local missingToken:TSyntaxToken = New TSyntaxToken(New TLexerToken(value, kind, missingTokenLocation, True), [], [])
		Return missingToken
	End Method
	
	Method GenerateMissingIdentifier:TSyntaxToken()
		Return GenerateMissingToken(TTokenKind.Identifier, "<missing identifier>")
	End Method
	
	Method GenerateMissingName:TNameSyntax()
		Return New TNameSyntax(GenerateMissingIdentifier())
	End Method
	
	Method GenerateMissingQualifiedName:TQualifiedNameSyntax()
		New TQualifiedNameSyntax([New TQualifiedNamePartSyntax(Null, GenerateMissingIdentifier())])
	End Method
	
	Method GenerateMissingExpression:TNameExpressionSyntax() ' TODO: TErrorExpression?
		Return New TNameExpressionSyntax(GenerateMissingName())
	End Method
	
	Method GenerateMissingType:TTypeSyntax()
		Return New TTypeSyntax(Null, New TQualifiedNameTypeBaseSyntax(GenerateMissingQualifiedName()), [])
	End Method
	
	Method ReportError(error:String)
		' TODO: allow specifying the code location of the error
		parseErrors.AddLast New TParseError(currentToken, error)
		'Throw error ' for debugging
	End Method
	
	' all Parse* methods either succeed by returning a valid object (in which case they can still
	' report errors) or they fail by returning Null (in which case they must return with the
	' parser set to the same state as upon entering so as to avoid tokens getting lost)
	' most Parse* methods immediately either fail or lock themselves into success after examining
	' the first token at the current position (making failure cheap), but some backtrack
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Top-Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
	
	Method ParseCompilationUnit:TCompilationUnitSyntax() Override ' always succeeds
		' TODO: should include directives be expanded during lexing, parsing, or semantic analysis?
		'       the advantage of lexing is not having to worry about header vs. body content
		'       and may thus allow making imports more flexible (usable anywhere?),
		'       the advantage of parsing is not having to build multi-file handling into the lexer
		'       and being able to have the expanded code in the syntax tree
		'       the advantage of semantic analysis is keeping both the lexer and the parser simpler
		'       and being able to represent include directives as simple nodes in the syntax tree
		
		Local header:TCodeHeaderSyntax = ParseCodeHeader()
		Local body:TCodeBodySyntax = ParseCodeBody()
		Local eofToken:TSyntaxToken = TakeToken(TTokenKind.Eof)
		Return New TCompilationUnitSyntax(header, body, eofToken)
	End Method
	
	Method ParseCodeHeader:TCodeHeaderSyntax() ' always succeeds
		Local elements:ICodeHeaderElementSyntax[]
		Repeat
			Local element:ICodeHeaderElementSyntax = ParseCodeHeaderElement()
			If Not element Then Exit
			elements :+ [element]
		Forever
		Return New TCodeHeaderSyntax(elements)
	End Method
	
	Method ParseCodeBody:TCodeBodySyntax() ' always succeeds
		Local block:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Eof])
		Return New TCodeBodySyntax(block)
	End Method
	
	Method ParseCodeBlock:TCodeBlockSyntax(expectedTerminatorKinds:TTokenKind[]) ' always succeeds
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
		
		Local elements:ICodeBlockElementSyntax[]
		Repeat
			Local element:ICodeBlockElementSyntax = ParseCodeBlockElement()
			If element Then
				If skippedTokens Then
					elements :+ [New TErrorSyntax(skippedTokens)]
					skippedTokens = []
				End If
				elements :+ [element]
			Else
				If terminatorStack.Contains(currentToken.Kind()) Then
					Exit
				Else
					ReportError "Unexpected token " + currentToken.lexerToken.value
					skippedTokens :+ [currentToken]
					AdvanceToNextSyntaxToken
				End If
			End If
		Forever
		
		terminatorStack.Pop
		
		Return New TCodeBlockSyntax(elements)
	End Method
	
	Method ParseCodeHeaderElement:ICodeHeaderElementSyntax()
		Local separator:TStatementSeparatorSyntax = ParseStatementSeparator()
		If separator Then
			Return separator
		Else
			Return ParseHeaderDirective()
		End If
	End Method
	
	Method ParseCodeBlockElement:ICodeBlockElementSyntax()
		Local element:ICodeBlockElementSyntax
		element = ParseStatementSeparator();     If element Then Return element
		element = ParseExternBlock();            If element Then Return element
		element = ParseVisibilityDirective();    If element Then Return element
		element = ParseDeclarationOrStatement(); If element Then Return element
		Return Null
	End Method
	
	Method ParseDeclarationOrStatement:ICodeBlockElementSyntax()
		Local declarationOrStatement:ICodeBlockElementSyntax
		declarationOrStatement = ParseDeclaration(); If declarationOrStatement Then Return declarationOrStatement
		declarationOrStatement = ParseStatement();   If declarationOrStatement Then Return declarationOrStatement
		Return Null
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Header Directives ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseHeaderDirective:IHeaderDirectiveSyntax()
		Local headerDirective:IHeaderDirectiveSyntax
		headerDirective = ParseStrictnessDirective(); If headerDirective Then Return headerDirective
		headerDirective = ParseModuleDirective();     If headerDirective Then Return headerDirective
		headerDirective = ParseModuleInfoDirective(); If headerDirective Then Return headerDirective
		headerDirective = ParseFrameworkDirective();  If headerDirective Then Return headerDirective
		headerDirective = ParseImportDirective();     If headerDirective Then Return headerDirective
		Return Null
	End Method
	
	Method ParseStrictnessDirective:TStrictnessDirectiveSyntax()
		Local strictness:TSyntaxToken = TryTakeToken([TTokenKind.Strict_, TTokenKind.SuperStrict_])
		If Not strictness Then Return Null
		
		Return New TStrictnessDirectiveSyntax(strictness)
	End Method
	
	Method ParseModuleDirective:TModuleDirectiveSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Module_)
		If Not keyword Then Return Null
		
		Local moduleName:TQualifiedNameSyntax = ParseModuleName()
		If Not moduleName Then
			ReportError "Expected module name"
			moduleName = GenerateMissingQualifiedName()
		End If
		
		Return New TModuleDirectiveSyntax(keyword, moduleName)
	End Method
	
	Method ParseModuleInfoDirective:TModuleInfoDirectiveSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.ModuleInfo_)
		If Not keyword Then Return Null
		
		Local info:TStringLiteralExpressionSyntax = ParseStringLiteralExpression()
		If Not info Then
			ReportError "Expected module info"
			info = New TStringLiteralExpressionSyntax(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
		End If
		
		Return New TModuleInfoDirectiveSyntax(keyword, info)
	End Method
	
	Method ParseFrameworkDirective:TFrameworkDirectiveSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Framework_)
		If Not keyword Then Return Null
		
		Local moduleName:TQualifiedNameSyntax = ParseModuleName()
		If Not moduleName Then
			ReportError "Expected module name"
			moduleName = GenerateMissingQualifiedName()
		End If
		
		Return New TFrameworkDirectiveSyntax(keyword, moduleName)
	End Method
	
	Method ParseImportDirective:TImportDirectiveSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Import_)
		If Not keyword Then Return Null
		
		Local fileName:TStringLiteralExpressionSyntax = ParseStringLiteralExpression()
		If fileName Then Return New TImportDirectiveSyntax(keyword, New TImportNameSyntax(fileName))
		
		Local moduleName:TQualifiedNameSyntax = ParseModuleName()
		If moduleName Then Return New TImportDirectiveSyntax(keyword, New TImportNameSyntax(moduleName))
		
		ReportError "Expected file or module name"
		Return New TImportDirectiveSyntax(keyword, New TImportNameSyntax(GenerateMissingQualifiedName()))
	End Method
	
	Method ParseModuleName:TQualifiedNameSyntax()
		Return ParseQualifiedName()
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseExternBlock:TExternBlockSyntax()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Extern_)
		If Not initiatorKeyword Then Return Null
		
		Local callingConvention:TStringLiteralExpressionSyntax = ParseStringLiteralExpression()
		
		Local elements:IExternBlockElementSyntax[]
		Repeat
			elements :+ ParseStatementSeparators()
			Local element:IExternBlockElementSyntax = ParseExternBlockElement()
			If Not element Then Exit
			' TODO: skip tokens until it is possible to parse an element or End Extern - or possibly the terminator token of a surrounding block?
		Forever
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(TTokenKind.EndExtern_)
		Return New TExternBlockSyntax(initiatorKeyword, callingConvention, elements, terminatorKeyword)
	End Method
	
	Method ParseExternBlockElement:IExternBlockElementSyntax()
		Local externBlockElement:IExternBlockElementSyntax
		externBlockElement = ParseExternTypeDeclaration();     If externBlockElement Then Return externBlockElement
		externBlockElement = ParseExternFunctionDeclaration(); If externBlockElement Then Return externBlockElement
		externBlockElement = ParseExternVariableDeclaration(); If externBlockElement Then Return externBlockElement
		Return Null
	End Method
	
	Method ParseDeclaration:IDeclarationSyntax()
		Local declaration:IDeclarationSyntax
		
		declaration = ParseTypeDeclaration();              If declaration Then Return declaration
		declaration = ParseCallableDeclaration();          If declaration Then Return declaration
		declaration = ParseCodeBlockVariableDeclaration(); If declaration Then Return declaration
		declaration = ParseTypeAliasDeclaration();         If declaration Then Return declaration
		
		declaration = ParseLabelDeclaration();             If declaration Then Return declaration
		
		Return Null
	End Method
	
	Method ParseTypeDeclaration:TTypeDeclarationSyntax()
		Local typeDeclaration:TTypeDeclarationSyntax
		typeDeclaration = ParseClassOrStructOrInterfaceDeclaration(); If typeDeclaration Then Return typeDeclaration
		typeDeclaration = ParseEnumDeclaration();                     If typeDeclaration Then Return typeDeclaration
		Return Null
	End Method
	
	Method ParseClassOrStructOrInterfaceDeclaration:TTypeDeclarationSyntax()
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
		
		Local name:TNameSyntax = ParseName()
		If Not name Then
			ReportError "Expected name"
			name = GenerateMissingName()
		End If
		
		Local extendsKeyword:TSyntaxToken
		Local superClass:TTypeSyntax
		Local superInterfaces:TTypeListSyntax
		Local implementsKeyword:TSyntaxToken
		
		Local expectMemberImplementations:Int
		Select kind
			Case ETypeKind.Class
				expectMemberImplementations = True
				extendsKeyword = TryTakeToken(TTokenKind.Extends_)
				If extendsKeyword Then
					superClass = ParseType(False)
					If Not superClass Then
						ReportError "Expected type"
					End If
				End If
				implementsKeyword = TryTakeToken(TTokenKind.Implements_)
				If implementsKeyword Then
					superInterfaces = ParseTypeList()
				End If
			Case ETypeKind.Struct_
				expectMemberImplementations = True
				If currentToken.Kind() = TTokenKind.Extends_ Or currentToken.Kind() = TTokenKind.Implements_ Then
					ReportError "Structs cannot extend or implement other types"
					' TODO
				End If
			Case ETypeKind.Interface_
				expectMemberImplementations = False
				extendsKeyword = TryTakeToken(TTokenKind.Extends_)
				If extendsKeyword Then
					superInterfaces = ParseTypeList()
					If Not superInterfaces Then
						ReportError "Expected type"
						superInterfaces = New TTypeListSyntax([])
					End If
				End If
			Default RuntimeError "Missing case"
		End Select
		
		Local modifiers:TTypeModifierSyntax[]
		Repeat
			Local modifierToken:TSyntaxToken = TryTakeToken([TTokenKind.Abstract_, TTokenKind.Final_]) ' NoDebug
			If modifierToken Then modifiers :+ [New TTypeModifierSyntax(modifierToken)] Else Exit
		Forever
		
		Local metaData:TMetaDataSyntax = ParseMetaData()
		
		Local members:ICodeBlockElementSyntax[] ' TODO: unify this with the parsing of code blocks, they can also contain visibility modifiers
		Repeat
			members :+ ParseStatementSeparators()
			Local member:ICodeBlockElementSyntax = ParseTypeMember(expectMemberImplementations)
			If member Then members :+ [member] Else Exit
		Forever
		Local body:TCodeBlockSyntax = New TCodeBlockSyntax(members)
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(terminatorTokenKind)
		
		Select kind
			Case ETypeKind.Class Return New TClassDeclarationSyntax(initiatorKeyword, name, extendsKeyword, superClass, implementsKeyword, superInterfaces, modifiers, metaData, body, terminatorKeyword)
			Case ETypeKind.Struct_ Throw "TODO"
			Case ETypeKind.Interface_ Throw "TODO"
			Default RuntimeError "Missing case"
		End Select
	End Method
	
	Method ParseExternClassDeclaration:TExternClassDeclarationSyntax()
		Throw "TODO"
	End Method
	
	Method ParseExternStructDeclaration:TExternTypeDeclarationSyntax()
		Throw "TODO"
	End Method
	
	Method ParseEnumDeclaration:TTypeDeclarationSyntax()
		'Local kind:ETypeKind = ETypeKind.Enum_
		Local declarationTokenKind:TTokenKind = TTokenKind.Enum_
		Local terminatorTokenKind:TTokenKind = TTokenKind.EndEnum_
				
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(declarationTokenKind)
		If Not initiatorKeyword Then Return Null
		
		Local name:TNameSyntax = ParseName()
		If Not name Then
			ReportError "Expected name"
			name = GenerateMissingName()
		End If
		
		Local baseType:TTypeSyntax = ParseType(True)
		
		Local flagsKeyword:TContextualKeywordSyntax = ParseContextualKeyword("Flags")
		
		Local metaData:TMetaDataSyntax = ParseMetaData()
		
		Local members:IEnumMemberSyntax[]
		Repeat
			members :+ ParseStatementSeparators()
			Local member:TEnumMemberDeclarationSyntax = ParseEnumMemberDeclaration()
			If member Then members :+ [member] Else Exit
		Forever
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(terminatorTokenKind)
		
		Return New TEnumDeclarationSyntax(initiatorKeyword, name, baseType, flagsKeyword, metaData, members, terminatorKeyword)
	End Method
	
	Method ParseEnumMemberDeclaration:TEnumMemberDeclarationSyntax()
		Local name:TNameSyntax = ParseName()
		If Not name Then Return Null
		
		Local assignment:TAssignmentSyntax = ParseConstantAssignment()
		
		Return New TEnumMemberDeclarationSyntax(name, assignment)
	End Method
	
	Method ParseExternTypeDeclaration:TExternTypeDeclarationSyntax()
		Local externTypeDeclaration:TExternTypeDeclarationSyntax
		externTypeDeclaration = ParseExternClassDeclaration();  If externTypeDeclaration Then Return externTypeDeclaration
		externTypeDeclaration = ParseExternStructDeclaration(); If externTypeDeclaration Then Return externTypeDeclaration
		Return Null
	End Method
	
	Method ParseTypeMember:ICodeBlockElementSyntax(expectImplementation:Int)
		Local typeMember:ICodeBlockElementSyntax
		typeMember = ParseVisibilityDirective();                               If typeMember Then Return typeMember
		
		typeMember = ParseTypeDeclaration();                                   If typeMember Then Return typeMember
		typeMember = ParseTypeMemberCallableDeclaration(expectImplementation); If typeMember Then Return typeMember
		typeMember = ParseTypeMemberVariableDeclaration();                     If typeMember Then Return typeMember
		typeMember = ParseTypeAliasDeclaration();                              If typeMember Then Return typeMember
		Return Null
	End Method
	
	Method ParseExternTypeMember:TExternDeclarationSyntax()
		Throw "TODO"
		
		Local externTypeMember:TExternDeclarationSyntax
		externTypeMember = ParseExternTypeDeclaration();     If externTypeMember Then Return externTypeMember
		externTypeMember = ParseExternFunctionDeclaration(); If externTypeMember Then Return externTypeMember
		externTypeMember = ParseExternVariableDeclaration(); If externTypeMember Then Return externTypeMember
		Return Null
	End Method
	
	Method ParseVisibilityDirective:TVisibilityDirectiveSyntax()
		Local visibility:TSyntaxToken = TryTakeToken([TTokenKind.Public_, TTokenKind.Protected_, TTokenKind.Private_])
		If Not visibility Then Return Null
		
		Return New TVisibilityDirectiveSyntax(visibility)
	End Method
	
	Method ParseCallableDeclaration:TCallableDeclarationSyntax()
		Return ParseTypeMemberCallableDeclaration(True) ' TODO: non-member functions cannot be abstract
	End Method
	
	Method ParseTypeMemberCallableDeclaration:TCallableDeclarationSyntax(expectImplementation:Int)
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
		
		Local name:TCallableDeclarationNameSyntax
		If operatorKeyword Then
			Local operatorName:TOperatorSyntax
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
				name = New TCallableDeclarationNameSyntax(operatorName)
			Else
				ReportError "Expected overloadable operator"
				name = New TCallableDeclarationNameSyntax(GenerateMissingName())
			End If
		Else
			Local identifierName:TNameSyntax = ParseName()
			If identifierName Then
				name = New TCallableDeclarationNameSyntax(identifierName)
			Else
				Local keywordName:TSyntaxToken = TryTakeToken([TTokenKind.New_, TTokenKind.Delete_])
				If keywordName Then
					name = New TCallableDeclarationNameSyntax(keywordName)
				Else
					ReportError "Expected " + initiatorKeyword.lexerToken.value.ToLower() + " name"
					name = New TCallableDeclarationNameSyntax(GenerateMissingName())
				End If
			End If
		End If
		
		' TODO type params
		
		Local type_:TTypeSyntax = ParseType(True)
		' generate a void-return-no-parameters type if no type can be parsed;
		' just append an empty parameter list if a type was parsed but it isn't a callable type
		If Not type_ Then
			ReportError "Expected callable type"
			type_ = New TTypeSyntax(Null, Null, [New TCallableTypeSuffixSyntax(GenerateMissingToken(TTokenKind.LParen), Null, GenerateMissingToken(TTokenKind.RParen))])
		Else If Not type_.suffixes Or Not (TCallableTypeSuffixSyntax(type_.suffixes[type_.suffixes.length - 1])) Then
			ReportError "Expected callable type"
			type_ = New TTypeSyntax(type_.colon, type_.base, type_.suffixes + [New TCallableTypeSuffixSyntax(TakeToken(TTokenKind.LParen), Null, TakeToken(TTokenKind.RParen))]) ' TODO: do not use TakeToken here to avoid duplicate errors?
		End If
		
		Local modifiers:TCallableModifierSyntax[]
		Repeat
			Local modifierToken:TSyntaxToken = TryTakeToken([TTokenKind.Abstract_, TTokenKind.Final_, TTokenKind.Override_, TTokenKind.Default_, TTokenKind.Inline_]) ' NoDebug
			If modifierToken Then
				modifiers :+ [New TCallableModifierSyntax(modifierToken)]
			Else
				Exit
			End If
		Forever
		
		Local metaData:TMetaDataSyntax = ParseMetaData()
		
		Local body:TCodeBlockSyntax
		Local terminatorKeyword:TSyntaxToken
		If (expectImplementation And Not ContainsKind(modifiers, TTokenKind.Abstract_)) Or (Not expectImplementation And ContainsKind(modifiers, TTokenKind.Default_)) Then
			body = ParseCodeBlock([terminatorTokenKind])
			terminatorKeyword = TakeToken(terminatorTokenKind)
		End If
		
		Return New TCallableDeclarationSyntax(initiatorKeyword, operatorKeyword, name, type_, modifiers, metaData, body, terminatorKeyword)
		
		Function ContainsKind:Int(array:TCallableModifierSyntax[], modifierKind:TTokenKind)
			For Local m:TCallableModifierSyntax = EachIn array
				If m.token.Kind() = modifierKind Then Return True
			Next
			Return False
		End Function
	End Method
	
	Method ParseExternFunctionDeclaration:TExternFunctionDeclarationSyntax()
		Throw "TODO"
	End Method
	
	Method ParseVariableDeclaration:TVariableDeclarationSyntax(acceptedDeclarationKeywords:TTokenKind[], multipleDeclaratorsOption:EMultipleDeclaratorsOption, initializersOption:EInitializersOption)
		' always succeeds if acceptedDeclarationKeywords contains Null
		' if acceptedDeclarationKeywords contains a Null element, a declaration will be accepted without any declaration keyword
		' in the case of a Const declaration being parsed, initializersOption is ignored and treated as Require
		Assert acceptedDeclarationKeywords Else "Must specify accepted keywords"
		? Debug
		For Local i:Int = 0 Until acceptedDeclarationKeywords.length
			Select acceptedDeclarationKeywords[i]
				Case TTokenKind.Local_, TTokenKind.Global_, TTokenKind.Const_, TTokenKind.Field_, Null ' ok
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
		
		Local modifiers:TVariableModifierSyntax[]
		Repeat
			Local modifierToken:TSyntaxToken = TryTakeToken(TTokenKind.ReadOnly_)
			If modifierToken Then
				modifiers :+ [New TVariableModifierSyntax(modifierToken)]
			Else
				Exit
			End If
		Forever
		If declarationKeyword And declarationKeyword.Kind() = TTokenKind.Const_ Then
			initializersOption = EInitializersOption.Require ' always require an initializer for Const declarations
		End If
		
		Local declarators:TVariableDeclaratorListSyntax
		If multipleDeclaratorsOption = EMultipleDeclaratorsOption.Allow Then
			declarators = ParseVariableDeclaratorList(initializersOption)
		Else
			Local declarator:TVariableDeclaratorSyntax = ParseVariableDeclarator(initializersOption)
			If declarator Then
				declarators = New TVariableDeclaratorListSyntax([New TVariableDeclaratorListElementSyntax(Null, declarator)])
			Else
				ReportError "Expected variable name"
				declarators = New TVariableDeclaratorListSyntax([])
			End If
		End If
		
		Local metaData:TMetaDataSyntax = ParseMetaData() ' TODO: not for locals (except params?)
		
		Return New TVariableDeclarationSyntax(declarationKeyword, modifiers, declarators, metaData)
	End Method
	
	Method ParseVariableDeclarator:TVariableDeclaratorSyntax(initializersOption:EInitializersOption)
		Local name:TNameSyntax = ParseName()
		If Not name Then Return Null
		
		Local type_:TTypeSyntax = ParseVariableDeclarationType()
		If Not type_ Then
			ReportError "Expected type"
		End If
		
		Local initializer:TAssignmentSyntax
		If initializersOption <> EInitializersOption.Disallow Then
			initializer = ParseAssignment()
		End If
		If Not initializer And initializersOption = EInitializersOption.Require Then
			ReportError "Expected initializer"
			initializer = New TAssignmentSyntax(GenerateMissingToken(TTokenKind.Eq), GenerateMissingExpression())
		End If
		
		If Not type_ And Not initializer Then
			type_ = GenerateMissingType()
			' if the type is missing but an initializer is present, it can be treated as an inference
			' assignment (the type of the variable will later be inferred from the type of the expression)
		End If
		
		Return New TVariableDeclaratorSyntax(name, type_, initializer)
	End Method
	
	Method ParseCodeBlockVariableDeclaration:TVariableDeclarationSyntax()
		Return ParseVariableDeclaration([TTokenKind.Local_, TTokenKind.Global_, TTokenKind.Const_], EMultipleDeclaratorsOption.Allow, EInitializersOption.Allow)
	End Method
	
	Method ParseParameterVariableDeclaration:TVariableDeclarationSyntax()
		Return ParseVariableDeclaration([TTokenKind(Null)], EMultipleDeclaratorsOption.Allow, EInitializersOption.Allow)
	End Method
	
	Method ParseTypeMemberVariableDeclaration:TVariableDeclarationSyntax()
		Return ParseVariableDeclaration([TTokenKind.Field_, TTokenKind.Global_, TTokenKind.Const_], EMultipleDeclaratorsOption.Allow, EInitializersOption.Allow)
	End Method
	
	Method ParseForCounterVariableDeclaration:TVariableDeclarationSyntax()
		Return ParseVariableDeclaration([TTokenKind.Local_], EMultipleDeclaratorsOption.Disallow, EInitializersOption.Disallow)
	End Method
	
	Method ParseCatchVariableDeclaration:TVariableDeclarationSyntax()
		Return ParseVariableDeclaration([TTokenKind(Null)], EMultipleDeclaratorsOption.Disallow, EInitializersOption.Disallow)
	End Method
	
	Method ParseExternVariableDeclaration:TExternVariableDeclarationSyntax()
		Throw "TODO" ' TODO
	End Method
	
	Method ParseTypeAliasDeclaration:IDeclarationSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Alias_)
		If Not keyword Then Return Null
		
		Throw "TODO" ' TODO
	End Method
	
	Method ParseLabelDeclaration:TLabelDeclarationSyntax()
		Local hash:TSyntaxToken = TryTakeToken(TTokenKind.FloatSigil)
		If Not hash Then Return Null
		
		Local name:TNameSyntax = ParseName()
		If Not name Then
			ReportError "Expected name"
			name = GenerateMissingName()
		End If
		
		Return New TLabelDeclarationSyntax(hash, name)
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Statements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseStatement:IStatementSyntax()
		Local statement:IStatementSyntax
		
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
		
		Return Null
	End Method
	
	Method ParseIfStatement:TIfStatementSyntax()
		Local ifKeyword:TSyntaxToken = TryTakeToken(TTokenKind.If_)
		If Not ifKeyword Then Return Null
		
		Local condition:IExpressionSyntax = ParseExpression()
		If Not condition Then
			ReportError "Expected expression"
			condition = GenerateMissingExpression()
		End If
		
		Local thenKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Then_)
		
		Local parseIfBranchBody:TCodeBlockSyntax(self_:TParser)
		Local isSingleLineIf:Int = Not (currentToken.Kind() = TTokenKind.Linebreak Or currentToken.Kind() = TTokenKind.Semicolon)
		If isSingleLineIf Then
			Function ParseSingleLineIfBranchBody:TCodeBlockSyntax(self_:TParser)
				Local blockElements:ICodeBlockElementSyntax[]
				Repeat
					Local separator:TStatementSeparatorSyntax = self_.ParseStatementSeparator()
					If separator Then
						blockElements :+ [separator]
						If separator.token.Kind() = TTokenKind.Linebreak Then Exit
					Else
						Local statement:IStatementSyntax = self_.ParseStatement()
						If statement Then
							blockElements :+ [statement]
						Else
							Exit
						End If
					End If
				Forever
				Return New TCodeBlockSyntax(blockElements)
			End Function
			parseIfBranchBody = ParseSingleLineIfBranchBody
		Else
			Function ParseMultiLineIfBranchBody:TCodeBlockSyntax(self_:TParser)
				Return self_.ParseCodeBlock([TTokenKind.Else_, TTokenKind.ElseIf_, TTokenKind.EndIf_])
			End Function
			parseIfBranchBody = ParseMultiLineIfBranchBody
		End If
		
		Local thenBody:TCodeBlockSyntax = parseIfBranchBody(Self)
		Local thenBranch:TThenIfBranchSyntax = New TThenIfBranchSyntax(thenKeyword, thenBody)
		
		Local branches:TIfBranchSyntax[] = [thenBranch]
		Repeat
			Select currentToken.Kind()
				Case TTokenKind.ElseIf_
					Local elseIfKeyword:TSyntaxToken = TakeToken()
					Local condition:IExpressionSyntax = ParseExpression()
					If Not condition Then
						ReportError "Expected expression"
						condition = GenerateMissingExpression()
					End If
					Local thenKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Then_)
					Local body:TCodeBlockSyntax = parseIfBranchBody(Self)
					branches :+ [New TElseIfIfBranchSyntax(elseIfKeyword, condition, thenKeyword, body)]
				Case TTokenKind.Else_
					Local elseKeyword:TSyntaxToken = TakeToken()
					Local body:TCodeBlockSyntax = parseIfBranchBody(Self)
					branches :+ [New TElseIfBranchSyntax(elseKeyword, body)]
				Default Exit
			End Select
		Forever
		
		Local endIfKeyword:TSyntaxToken
		If Not isSingleLineIf Then endIfKeyword = TakeToken(TTokenKind.EndIf_)
		
		Return New TIfStatementSyntax(ifKeyword, condition, branches, endIfKeyword)
	End Method
	
	Method ParseSelectStatement:TSelectStatementSyntax()
		Local selectKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Select_)
		If Not selectKeyword Then Return Null
		
		Local expression:IExpressionSyntax = ParseExpression()
		If Not expression Then
			ReportError "Expected expression"
			expression = GenerateMissingExpression()
		End If
		
		Local statementSeparators:TStatementSeparatorSyntax[] = ParseStatementSeparators()
		
		Local branches:TSelectBranchSyntax[]
		Repeat
			Select currentToken.Kind()
				Case TTokenKind.Case_
					Local keyword:TSyntaxToken = TakeToken()
					Local expression:IExpressionSyntax = ParseExpression()
					If Not expression Then
						ReportError "Expected expression"
						expression = GenerateMissingExpression()
					End If
					Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Case_, TTokenKind.Default_, TTokenKind.EndSelect_])
					branches :+ [New TCaseSelectBranchSyntax(keyword, expression, body)]
				Case TTokenKind.Default_
					Local keyword:TSyntaxToken = TakeToken()
					Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Case_, TTokenKind.Default_, TTokenKind.EndSelect_])
					branches :+ [New TDefaultSelectBranchSyntax(keyword, body)]
				Default Exit
			End Select
		Forever
		
		Local endSelectKeyword:TSyntaxToken = TakeToken(TTokenKind.EndSelect_)
		
		Return New TSelectStatementSyntax(selectKeyword, expression, statementSeparators, branches, endSelectKeyword)
	End Method
	
	Method ParseForStatement:TForStatementSyntax()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.For_)
		If Not initiatorKeyword Then Return Null
		
		Local counter:TForCounterSyntax
			Local counterDeclaration:TVariableDeclarationSyntax = ParseForCounterVariableDeclaration()
		If counterDeclaration Then
			counter = New TForCounterDeclarationSyntax(counterDeclaration)
		Else
			Local counterExpression:IExpressionSyntax = ParseLValue()
			If counterExpression Then
				counter = New TForCounterExpressionSyntax(counterExpression)
			Else
				ReportError "Expected declaration or expression"
				counter = New TForCounterExpressionSyntax(GenerateMissingExpression())
			End If
		End If
		
		Local eq:TSyntaxToken = TakeToken(TTokenKind.Eq)
		
		Local valueSequence:TForValueSequenceSyntax
		If currentToken.Kind() = TTokenKind.EachIn_ Then
			Local eachInKeyword:TSyntaxToken = TakeToken()
			Local iterableExpression:IExpressionSyntax = ParseExpression()
			If Not iterableExpression Then
				ReportError "Expected expression"
				iterableExpression = GenerateMissingExpression()
			End If
			valueSequence = New TForEachInValueSequenceSyntax(eachInKeyword, iterableExpression)
		Else
			Local expressionAfterEq:IExpressionSyntax = ParseExpression()
			If expressionAfterEq Then
				Local startExpression:IExpressionSyntax = expressionAfterEq
				If Not startExpression Then
					ReportError "Expected expression"
					startExpression = GenerateMissingExpression()
				End If
				
				Local keyword:TSyntaxToken = TryTakeToken([TTokenKind.To_, TTokenKind.Until_])
				If Not keyword Then
					ReportError "Expected To or Until"
					keyword = GenerateMissingToken(TTokenKind.To_)
				End If
				
				Local endExpression:IExpressionSyntax = ParseExpression()
				If Not endExpression Then
					ReportError "Expected expression"
					endExpression = GenerateMissingExpression()
				End If
				
				Select keyword.Kind()
					Case TTokenKind.To_    valueSequence = New TForToValueSequenceSyntax(startExpression, keyword, endExpression)
					Case TTokenKind.Until_ valueSequence = New TForUntilValueSequenceSyntax(startExpression, keyword, endExpression)
					Default RuntimeError "Missing case"
				End Select
			Else
				ReportError "Expected EachIn or expression"
				valueSequence = New TForEachInValueSequenceSyntax(GenerateMissingToken(TTokenKind.EachIn_), GenerateMissingExpression())
			End If
		End If
		
		Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Next_])
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(TTokenKind.Next_)
				
		Return New TForStatementSyntax(initiatorKeyword, counter, eq, valueSequence, body, terminatorKeyword)
	End Method
	
	Method ParseWhileStatement:TWhileStatementSyntax()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.While_)
		If Not initiatorKeyword Then Return Null
		
		Local condition:IExpressionSyntax = ParseExpression()
		If Not condition Then
			ReportError "Expected expression"
			condition = GenerateMissingExpression()
		End If
		
		Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Wend_])
		
		Local terminatorKeyword:TSyntaxToken = TakeToken(TTokenKind.Wend_)
		
		Return New TWhileStatementSyntax(initiatorKeyword, condition, body, terminatorKeyword)
	End Method
	
	Method ParseRepeatStatement:IStatementSyntax()
		Local initiatorKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Repeat_)
		If Not initiatorKeyword Then Return Null
		
		Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Until_, TTokenKind.Forever_])
		
		Local terminator:TRepeatTerminatorSyntax
		Local terminatorKeyword:TSyntaxToken = TryTakeToken([TTokenKind.Until_, TTokenKind.Forever_])
		If Not terminatorKeyword Then
			ReportError "Expected Until or Forever"
			terminatorKeyword = GenerateMissingToken(TTokenKind.Forever_)
		End If
		Select terminatorKeyword.Kind()
			Case TTokenKind.Until_
				Local condition:IExpressionSyntax = ParseExpression()
				If Not condition Then
					ReportError "Expected expression"
					condition = GenerateMissingExpression()
				End If
				terminator = New TRepeatUntilTerminatorSyntax(terminatorKeyword, condition)
			Case TTokenKind.Forever_
				terminator = New TRepeatForeverTerminatorSyntax(terminatorKeyword)
			Default RuntimeError "Missing case"
		End Select
				
		Return New TRepeatStatementSyntax(initiatorKeyword, body, terminator)
	End Method
	
	Method ParseExitStatement:TExitStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Exit_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntax = ParseName()
		
		Return New TExitStatementSyntax(keyword, labelName)
	End Method
	
	Method ParseContinueStatement:TContinueStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Continue_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntax = ParseName()
		
		Return New TContinueStatementSyntax(keyword, labelName)
	End Method
	
	Method ParseGotoStatement:TGotoStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Goto_)
		If Not keyword Then Return Null
		
		Local labelName:TNameSyntax = ParseName()
		If Not labelName Then
			ReportError "Expected label name"
			labelName = GenerateMissingName()
		End If
		
		Return New TGotoStatementSyntax(keyword, labelName)
	End Method
	
	Method ParseReturnStatement:TReturnStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Return_)
		If Not keyword Then Return Null
		
		Local expression:IExpressionSyntax = ParseExpression()
		
		Return New TReturnStatementSyntax(keyword, expression)
	End Method
	
	Method ParseTryStatement:TTryStatementSyntax()
		Local tryKeyword:TSyntaxToken = TryTakeToken(TTokenKind.Try_)
		If Not tryKeyword Then Return Null
		
		Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Catch_, TTokenKind.Finally_, TTokenKind.EndTry_])
		
		Local branches:TTryBranchSyntax[]
		Repeat
			Select currentToken.Kind()
				Case TTokenKind.Catch_
					Local keyword:TSyntaxToken = TakeToken()
					Local declaration:TVariableDeclarationSyntax = ParseCatchVariableDeclaration()
					If Not declaration Then
						ReportError "Expected variable declaration"
						declaration = New TVariableDeclarationSyntax(Null, [], New TVariableDeclaratorListSyntax([New TVariableDeclaratorListElementSyntax(Null, New TVariableDeclaratorSyntax(GenerateMissingName(), Null, Null))]), Null)
					End If
					Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Catch_, TTokenKind.Finally_, TTokenKind.EndTry_])
					branches :+ [New TCatchTryBranchSyntax(keyword, declaration, body)]
				Case TTokenKind.Finally_
					Local keyword:TSyntaxToken = TakeToken()
					Local body:TCodeBlockSyntax = ParseCodeBlock([TTokenKind.Catch_, TTokenKind.Finally_, TTokenKind.EndTry_])
					branches :+ [New TFinallyTryBranchSyntax(keyword, body)]
				Default Exit
			End Select
		Forever
		If Not branches Then ReportError "Expected Catch or Finally"
		
		Local endTryKeyword:TSyntaxToken = TakeToken(TTokenKind.EndTry_)
				
		Return New TTryStatementSyntax(tryKeyword, body, branches, endTryKeyword)
	End Method
	
	Method ParseThrowStatement:TThrowStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Throw_)
		If Not keyword Then Return Null
		
		Local expression:IExpressionSyntax = ParseExpression()
		If Not expression Then
			ReportError "Expected expression"
			expression = GenerateMissingExpression()
		End If
		
		Return New TThrowStatementSyntax(expression)
	End Method
	
	Method ParseAssertStatement:TAssertStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.Assert_)
		If Not keyword Then Return Null
		
		Local expression:IExpressionSyntax = ParseExpression()
		If Not expression Then
			ReportError "Expected expression"
			expression = GenerateMissingExpression()
		End If
		
		Local commaOrElse:TSyntaxToken = TryTakeToken([TTokenKind.Comma, TTokenKind.Else_])
		If Not commaOrElse Then
			ReportError "Expected comma or Else"
			commaOrElse = GenerateMissingToken(TTokenKind.Else_)
		End If
		
		Local message:TStringLiteralExpressionSyntax
		If Not message Then
			ReportError "Expected comma or Else"
			message = New TStringLiteralExpressionSyntax(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
		End If
		
		Return New TAssertStatementSyntax(keyword, expression, commaOrElse, message)
	End Method
	
	Method ParseEndStatement:TEndStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.End_)
		If Not keyword Then Return Null
		
		Return New TEndStatementSyntax(keyword)
	End Method
	
	Method ParseNativeCodeStatement:TNativeCodeStatementSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.NativeCode)
		If Not token Then Return Null
		
		Return New TNativeCodeStatementSyntax(token)
	End Method
	
	Method ParseDefDataStatement:IStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.DefData_)
		If Not keyword Then Return Null
		
		Throw "TODO" ' TODO
	End Method
	
	Method ParseReadDataStatement:IStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.ReadData_)
		If Not keyword Then Return Null
		
		Throw "TODO" ' TODO
	End Method
	
	Method ParseRestoreDataStatement:IStatementSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.RestoreData_)
		If Not keyword Then Return Null
		
		Throw "TODO" ' TODO
	End Method
	
	Method ParseExpressionBasedStatement:IStatementSyntax()
		' TODO: expensive backtracking here, improve
		' the order of these here is very important, see below
		Local expressionBasedStatement:IStatementSyntax
		expressionBasedStatement = ParseAssignmentStatement();    If expressionBasedStatement Then Return expressionBasedStatement
		expressionBasedStatement = ParseParenlessCallStatement(); If expressionBasedStatement Then Return expressionBasedStatement
		expressionBasedStatement = ParseExpressionStatement();    If expressionBasedStatement Then Return expressionBasedStatement
		Return Null
	End Method
	
	Method ParseAssignmentStatement:TAssignmentStatementSyntax() ' backtracks
		' this method parses code accepts whose first part would also be accepted by ParseParenlessCallStatement,
		' so in any context where both are valid, this method must be called first
		Local state:SParserState = SaveState()
		
		Local target:IExpressionSyntax = ParseLValue()
		If Not target Then Return Null
		
		Local assignment:TAssignmentSyntax = ParseAssignment()
		If Not assignment Then
			RestoreState state
			Return Null
		End If
		
		Return New TAssignmentStatementSyntax(target, assignment)
	End Method
	
	Method ParseLValue:IExpressionSyntax()
		' this must not attempt to parse a relational expression, otherwise there will be no = left over for the assignment
		Return ParsePostfixCompatibleExpression() ' TODO: exclude call expressions
	End Method
	
	Method ParseParenlessCallStatement:TParenlessCallStatementSyntax() ' backtracks
		' this method accepts code constructs whose first part would also be accepted by ParseExpressionStatement,
		' so in any context where both are valid, this method must be called first
		Local state:SParserState = SaveState()
		
		Local expression:IExpressionSyntax = ParsePostfixCompatibleExpression() ' TODO: exclude call expressions
		If Not expression Then Return Null
		If TCallExpressionSyntax(expression) Then
			RestoreState state
			Return Null
		End If
		
		If Not IsValidType(expression) Then
			RestoreState state
			Return Null
		End If
		Function IsValidType:Int(expression:IExpressionSyntax)
			'If TTypeBindingExpressionSyntax(expression) Then Return True
			If TMemberAccessExpressionSyntax(expression) Then Return True
			If TIndexExpressionSyntax(expression) Then Return True
			'If TTypeAssertionExpressionSyntax(expression) Then Return Return IsValidType(TTypeAssertionExpressionSyntax(expression).expression)
			If TParenExpressionSyntax(expression) Then Return IsValidType(TParenExpressionSyntax(expression).expression)
			If TNameExpressionSyntax(expression) Then Return True
			Return False
		End Function
		
		Local callOperator:TExpressionListSyntax = ParseExpressionList(True)
		
		Return New TParenlessCallStatementSyntax(expression, callOperator)
	End Method
	
	Method ParseExpressionStatement:TExpressionStatementSyntax() ' backtracks
		Local state:SParserState = SaveState()
		
		Local expression:IExpressionSyntax = ParsePostfixCompatibleExpression()
		If Not expression Then Return Null
		Assert Not (TMemberAccessExpressionSyntax(expression) Or TIndexExpressionSyntax(expression)) Else "ParseExpressionStatement parsed expression type that should have been parsed by ParseParenlessCallStatement" ' Or TTypeBindingExpressionSyntax Or TTypeAssertionExpressionSyntax
		
		If Not IsValidType(expression) Then
			RestoreState state
			Return Null
		End If
		Function IsValidType:Int(expression:IExpressionSyntax)
			If TCallExpressionSyntax(expression) Then Return True
			If TNewExpressionSyntax(expression) Then Return True
			If TParenExpressionSyntax(expression) Then Return IsValidType(TParenExpressionSyntax(expression).expression)
			Return False
		End Function
		
		Return New TExpressionStatementSyntax(expression)
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Expressions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseExpression:IExpressionSyntax()
		Return ParseRangeCompatibleExpression()
		' the more specialized Parse*Expression methods that return a IExpressionSyntax may return
		' any type of expression with higher precedence
		' for example, ParseSumExpression may return a TProductExpression if no sum operator is present
	End Method
	
	Method ParseRangeCompatibleExpression:IRangeCompatibleExpressionSyntax()
		' TODO: even if it means breaking backwards compatibility, this should probably have higher
		'       precedence than logical and relational operators
		'       relational operators other than = and <> should not be supported by ranges,
		'       and logical operators should probably not be supported by structs in general
		'       (assuming they cannot be overloaded someday), so the chances of breaking any
		'       code with that change would be pretty low
				
		' the range operator is unique in that its operands are optional
		Local lhs:IRangeCompatibleExpressionSyntax
		If currentToken.Kind() <> TTokenKind.DotDot Then
			lhs = ParseOrCompatibleExpression()
			If Not lhs Then Return Null
		End If
		' lhs can be Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.DotDot])
			If op Then
				Local rhs:IOrCompatibleExpressionSyntax = ParseOrCompatibleExpression()
				' rhs can be Null
				
				lhs = New TRangeExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseOrCompatibleExpression:IOrCompatibleExpressionSyntax()
		Local lhs:IOrCompatibleExpressionSyntax = ParseAndCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.Or_])
			If op Then
				Local rhs:IAndCompatibleExpressionSyntax = ParseAndCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TOrExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseAndCompatibleExpression:IAndCompatibleExpressionSyntax()
		Local lhs:IAndCompatibleExpressionSyntax = ParseRelationalCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.And_])
			If op Then
				Local rhs:IRelationalCompatibleExpressionSyntax = ParseRelationalCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TAndExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseRelationalCompatibleExpression:IRelationalCompatibleExpressionSyntax()
		Local lhs:IRelationalCompatibleExpressionSyntax = ParseUnionCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			' TODO: when introducing generics, remove token kind "Neq"?
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.Eq])
			If Not op Then op = ParseOperator([TTokenKind.Neq])
			If Not op Then op = ParseOperator([TTokenKind.Lt])
			If Not op Then op = ParseOperator([TTokenKind.Gt])
			If Not op Then op = ParseOperator([TTokenKind.Leq])
			If Not op Then op = ParseOperator([TTokenKind.Geq])
			If op Then
				Local rhs:IUnionCompatibleExpressionSyntax = ParseUnionCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TRelationalExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseUnionCompatibleExpression:IUnionCompatibleExpressionSyntax()
		Local lhs:IUnionCompatibleExpressionSyntax = ParseIntersectionCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.BitOr])
			If Not op Then op = ParseOperator([TTokenKind.BitNot])
			If op Then
				Local rhs:IIntersectionCompatibleExpressionSyntax = ParseIntersectionCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TUnionExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseIntersectionCompatibleExpression:IIntersectionCompatibleExpressionSyntax()
		Local lhs:IIntersectionCompatibleExpressionSyntax = ParseSumCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.BitAnd])
			If op Then
				Local rhs:ISumCompatibleExpressionSyntax = ParseSumCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TIntersectionExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseSumCompatibleExpression:ISumCompatibleExpressionSyntax()
		Local lhs:ISumCompatibleExpressionSyntax = ParseProductCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.Plus])
			If Not op Then op = ParseOperator([TTokenKind.Minus])
			If op Then
				Local rhs:IProductCompatibleExpressionSyntax = ParseProductCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TSumExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseProductCompatibleExpression:IProductCompatibleExpressionSyntax()
		Local lhs:IProductCompatibleExpressionSyntax = ParseExponentialCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.Mul])
			If Not op Then op = ParseOperator([TTokenKind.Div])
			If Not op Then op = ParseOperator([TTokenKind.Mod_])
			If Not op Then op = ParseOperator([TTokenKind.Shl_])
			If Not op Then op = ParseOperator([TTokenKind.Shr_])
			If Not op Then op = ParseOperator([TTokenKind.Sar_])
			If op Then
				Local rhs:IExponentialCompatibleExpressionSyntax = ParseExponentialCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TProductExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParseExponentialCompatibleExpression:IExponentialCompatibleExpressionSyntax()
		Local lhs:IExponentialCompatibleExpressionSyntax = ParsePrefixCompatibleExpression()
		If Not lhs Then Return Null
		
		Repeat
			Local op:TOperatorSyntax
			If Not op Then op = ParseOperator([TTokenKind.Pow])
			If op Then
				Local rhs:IPrefixCompatibleExpressionSyntax = ParsePrefixCompatibleExpression()
				If Not rhs Then
					ReportError "Expected expression"
					rhs = GenerateMissingExpression()
				End If
				
				lhs = New TExponentialExpressionSyntax(lhs, op, rhs)
			Else
				Return lhs
			End If
		Forever
	End Method
	
	Method ParsePrefixCompatibleExpression:IPrefixCompatibleExpressionSyntax()
		Local op:TOperatorSyntax
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
			Local arg:IPrefixCompatibleExpressionSyntax = ParsePrefixCompatibleExpression()
			If Not arg Then
				' TODO: is there a better way to handle this kind of error (skip the operator)?
				ReportError "Expected expression"
				arg = GenerateMissingExpression()
			End If
			
			Return New TPrefixOperatorExpressionSyntax(op, arg)
		Else
			Local castExpression:TTypeCastExpressionSyntax = ParseTypeCastExpression()
			If castExpression Then
				Return castExpression
			Else
				Return ParsePostfixCompatibleExpression()
			End If
		End If
	End Method
	
	Method ParseTypeCastExpression:TTypeCastExpressionSyntax()
		' TODO: integrate this into ParseType somehow
		Local state:SParserState = SaveState()
		
		Local base:TTypeBaseSyntax = ParseTypeBase()
		If Not base Then Return Null
		If TSigilTypeBaseSyntax(base) Then
			' not allowed in a cast
			RestoreState state
			Return Null		
		End If
		
		Local suffixes:TTypeSuffixSyntax[]
		Repeat
			Local suffix:TTypeSuffixSyntax = ParseTypeSuffix(False)
			If suffix Then suffixes :+ [suffix] Else Exit
		Forever
		
		If Not TKeywordTypeBaseSyntax(base) And Not suffixes Then
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
		
		Local targetType:TTypeSyntax = New TTypeSyntax(Null, base, suffixes)
		
		Local arg:IPrefixCompatibleExpressionSyntax = ParsePrefixCompatibleExpression()
		If Not arg Then
			ReportError "Expected expression"
			arg = GenerateMissingExpression()
		End If
		
		Return New TTypeCastExpressionSyntax(targetType, arg)
	End Method
	
	Method ParsePostfixCompatibleExpression:IPostfixCompatibleExpressionSyntax()
		Local arg:IPostfixCompatibleExpressionSyntax = ParsePrimaryExpression()
		If Not arg Then arg = ParseMemberAccessExpression(Null) ' global scope access (leading dot)
		If Not arg Then Return Null
		
		Return ParsePostfixCompatibleExpression(arg)
	End Method
	
	Method ParsePostfixCompatibleExpression:IPostfixCompatibleExpressionSyntax(arg:IPostfixCompatibleExpressionSyntax)
		Local postfixExpression:IPostfixCompatibleExpressionSyntax
		'postfixExpression = ParseTypeBindingExpression(arg);   If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression)
		postfixExpression = ParseMemberAccessExpression(arg);  If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression)
		postfixExpression = ParseIndexExpression(arg);         If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression)
		postfixExpression = ParseCallExpression(arg);          If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression)
		'postfixExpression = ParseTypeAssertionExpression(arg); If postfixExpression Then Return ParsePostfixCompatibleExpression(postfixExpression)
		Return arg
	End Method
		
	Method ParseTypeBindingExpression:IExpressionSyntax(arg:IPostfixCompatibleExpressionSyntax) ' backtracks
		Throw "TODO"
	End Method
	
	Method ParseMemberAccessExpression:TMemberAccessExpressionSyntax(arg:IPostfixCompatibleExpressionSyntax)
		Local dot:TSyntaxToken = TryTakeToken(TTokenKind.Dot)
		If Not dot Then Return Null
		
		Local memberName:TNameSyntax = ParseName()
		If Not memberName Then
			ReportError "Expected member name"
			memberName = GenerateMissingName()
		End If
		
		Return New TMemberAccessExpressionSyntax(arg, dot, memberName)
	End Method
	
	Method ParseIndexExpression:TIndexExpressionSyntax(arg:IPostfixCompatibleExpressionSyntax)
		Local indexOperator:TBracketExpressionListSyntax = ParseBracketExpressionList(False)
		If Not indexOperator Then Return Null
		
		Return New TIndexExpressionSyntax(arg, indexOperator)
	End Method
	
	Method ParseCallExpression:TCallExpressionSyntax(arg:IPostfixCompatibleExpressionSyntax)
		Local callOperator:TParenExpressionListSyntax = ParseParenExpressionList(True)
		If Not callOperator Then Return Null
		
		Return New TCallExpressionSyntax(arg, callOperator)
	End Method
	
	Method ParseTypeAssertionExpression:TTypeAssertionExpressionSyntax(arg:IPostfixCompatibleExpressionSyntax)
		Throw "TODO"
	End Method
	
	Method ParsePrimaryExpression:IPrimaryExpressionSyntax()
		Local primaryExpression:IPrimaryExpressionSyntax
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
	
	Method ParseParenExpression:TParenExpressionSyntax()
		Local lparen:TSyntaxToken = TryTakeToken(TTokenKind.LParen)
		If Not lparen Then Return Null
		
		Local innerExpression:IExpressionSyntax = ParseExpression()
		If Not innerExpression Then
			ReportError "Expected expression"
			innerExpression = GenerateMissingExpression()
		End If
		
		Local rparen:TSyntaxToken = TakeToken(TTokenKind.RParen)
		
		Return New TParenExpressionSyntax(lparen, innerExpression, rparen)
	End Method
	
	Method ParseNewExpression:TNewExpressionSyntax()
		Local keyword:TSyntaxToken = TryTakeToken(TTokenKind.New_)
		If Not keyword Then Return Null
		
		Local type_:TTypeSyntax = ParseNewConstructibleType()
		If Not type_ Then
			ReportError "Expected type"
			type_ = GenerateMissingType()
		End If
		
		Local callOperator:TParenExpressionListSyntax = ParseParenExpressionList(True)
				
		Return New TNewExpressionSyntax(keyword, type_, callOperator)
	End Method
	
	Method ParseSelfExpression:TSelfExpressionSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Self_)
		If Not token Then Return Null
		
		Return New TSelfExpressionSyntax(token)
	End Method
	
	Method ParseSuperExpression:TSuperExpressionSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Super_)
		If Not token Then Return Null
		
		Return New TSuperExpressionSyntax(token)
	End Method
	
	Method ParseNullExpression:TNullExpressionSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Null_)
		If Not token Then Return Null
		
		Return New TNullExpressionSyntax(token)
	End Method
	
	Method ParseTrueExpression:TTrueExpressionSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.True_)
		If Not token Then Return Null
		
		Return New TTrueExpressionSyntax(token)
	End Method
	
	Method ParseFalseExpression:TFalseExpressionSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.False_)
		If Not token Then Return Null
		
		Return New TFalseExpressionSyntax(token)
	End Method
	
	Method ParsePiExpression:TPiExpressionSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Pi_)
		If Not token Then Return Null
		
		Return New TPiExpressionSyntax(token)
	End Method
	
	Method ParseNameExpression:TNameExpressionSyntax()
		Local name:TNameSyntax = ParseName()
		If Not name Then Return Null
		
		Return New TNameExpressionSyntax(name)
	End Method
	
	Method ParseLiteralExpression:TLiteralExpressionSyntax()
		Local literalExpression:TLiteralExpressionSyntax
		literalExpression = ParseArrayLiteralExpression();   If literalExpression Then Return literalExpression
		literalExpression = ParseNumericLiteralExpression(); If literalExpression Then Return literalExpression
		literalExpression = ParseStringLiteralExpression();  If literalExpression Then Return literalExpression
		Return Null
	End Method
	
	Method ParseNumericLiteralExpression:TNumericLiteralExpressionSyntax()
		Local value:TSyntaxToken = TryTakeToken([TTokenKind.IntLiteral, TTokenKind.HexIntLiteral, TTokenKind.BinIntLiteral, TTokenKind.FloatLiteral])
		If Not value Then Return Null
		
		Local type_:TTypeSyntax = ParseNonCallableType(True)
		
		Return New TNumericLiteralExpressionSyntax(value, type_)
	End Method
	
	Method ParseStringLiteralExpression:TStringLiteralExpressionSyntax()
		Local value:TSyntaxToken = TryTakeToken(TTokenKind.StringLiteral)
		If Not value Then Return Null
		
		Local type_:TTypeSyntax = ParseNonCallableType(True)
		
		Return New TStringLiteralExpressionSyntax(value, type_)
	End Method
	
	Method ParseArrayLiteralExpression:TArrayLiteralExpressionSyntax()
		Local elementList:TBracketExpressionListSyntax = ParseBracketExpressionList(False)
		If Not elementList Then Return Null
		
		Return New TArrayLiteralExpressionSyntax(elementList)
	End Method
	
	Method ParseConstantExpression:IExpressionSyntax()
		Return ParseExpression()
	End Method
	
	' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Auxiliary Constructs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	Method ParseType:TTypeSyntax(withColon:Int) ' backtracks
		Local state:SParserState = SaveState()
		
		Local colon:TSyntaxToken
		Local base:TTypeBaseSyntax
		
		If withColon And currentToken.Kind() <> TTokenKind.LParen Then
			colon = TryTakeToken(TTokenKind.Colon)
			If Not colon Then Return Null
		End If
		
		If Not colon And currentToken.Kind() = TTokenKind.LParen Then
			base = Null	' void-returning callable
		Else
			base = ParseTypeBase()
			If Not base Then
				RestoreState state
				Return Null
			End If
		End If
		
		Local suffixes:TTypeSuffixSyntax[]
		Repeat
			' TODO: handle marshalled types like $z and $w
			Local suffix:TTypeSuffixSyntax = ParseTypeSuffix(True)
			If suffix Then suffixes :+ [suffix] Else Exit
		Forever
		
		Return New TTypeSyntax(colon, base, suffixes)
	End Method
	
	Method ParseNewConstructibleType:TTypeSyntax()
		Return ParseType(False)
		' TODO
		' like ParseType, but does not allow:
		' - function types
		' - array types without dimensions
		' - Ptr and Var types
		' but allows:
		' - array dimensions
		' or should it actually allow every type? a parameterless New could be considered equivalent,
		' and this method being called after we are already locked into parsing a type to Null
	End Method
	
	Method ParseNonCallableType:TTypeSyntax(withColon:Int)
		Return ParseType(True)
		' TODO
		' like ParseType, but does not allow:
		' - function types
		' used for type assertions and casts
	End Method
	
	Method ParseVariableDeclarationType:TTypeSyntax()
		Return ParseType(True)
		' TODO
		' like ParseType, but allows for array dimensions (like ParseNewConstructibleType does)
	End Method
		
	Method ParseTypeBase:TTypeBaseSyntax()
		Local typeBase:TTypeBaseSyntax
		typeBase = ParseQualifiedNameTypeBase(); If typeBase Then Return typeBase
		' TODO: ParseTypeBindingOperator?
		typeBase = ParseSigilTypeBase();         If typeBase Then Return typeBase
		typeBase = ParseKeywordTypeBase();       If typeBase Then Return typeBase
		Return Null
	End Method
	
	Method ParseKeywordTypeBase:TKeywordTypeBaseSyntax()
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
		
		Return New TKeywordTypeBaseSyntax(keyword)
	End Method
	
	Method ParseSigilTypeBase:TSigilTypeBaseSyntax()
		Local sigil:TSyntaxToken = TryTakeToken([ ..
			TTokenKind.ByteSigil, ..
			TTokenKind.IntSigil, ..
			TTokenKind.FloatSigil, ..
			TTokenKind.DoubleSigil, ..
			TTokenKind.StringSigil ..
		])
		If Not sigil Then Return Null
		
		Return New TSigilTypeBaseSyntax(sigil)
	End Method
	
	Method ParseQualifiedNameTypeBase:TQualifiedNameTypeBaseSyntax()
		Local name:TQualifiedNameSyntax = ParseQualifiedName()
		If Not name Then Return Null
		
		Return New TQualifiedNameTypeBaseSyntax(name)
	End Method
	
	Method ParseTypeSuffix:TTypeSuffixSyntax(allowCallable:Int)
		Local typeSuffix:TTypeSuffixSyntax
		If allowCallable Then
			typeSuffix = ParseCallableTypeSuffix(); If typeSuffix Then Return typeSuffix
		End If
		typeSuffix = ParseArrayTypeSuffix(); If typeSuffix Then Return typeSuffix
		typeSuffix = ParsePtrTypeSuffix();   If typeSuffix Then Return typeSuffix
		typeSuffix = ParseVarTypeSuffix();   If typeSuffix Then Return typeSuffix
		Return Null
	End Method
	
	Method ParsePtrTypeSuffix:TPtrTypeSuffixSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Ptr_)
		If Not token Then Return Null
		
		Return New TPtrTypeSuffixSyntax(token)
	End Method
	
	Method ParseVarTypeSuffix:TVarTypeSuffixSyntax()
		Local token:TSyntaxToken = TryTakeToken(TTokenKind.Var_)
		If Not token Then Return Null
		
		Return New TVarTypeSuffixSyntax(token)
	End Method
	
	Method ParseArrayTypeSuffix:TArrayTypeSuffixSyntax()
		' TODO: distinguish between New (elements may be non-empty) and regular array type (elements must be empty)
		'       this is very important in expressions to tell casts apart from indexing
		Local lbracket:TSyntaxToken = TryTakeToken(TTokenKind.LBracket)
		If Not lbracket Then Return Null
		
		Local dimensionList:TExpressionListSyntax = ParseExpressionList(True)
		
		Local rbracket:TSyntaxToken = TakeToken(TTokenKind.RBracket)
		
		Return New TArrayTypeSuffixSyntax(lbracket, dimensionList, rbracket)
	End Method
	
	Method ParseCallableTypeSuffix:TCallableTypeSuffixSyntax()
		Local lparen:TSyntaxToken = TryTakeToken(TTokenKind.LParen)
		If Not lparen Then Return Null
		
		Local parameterDeclaration:TVariableDeclarationSyntax = ParseParameterVariableDeclaration()
		
		Local rparen:TSyntaxToken = TakeToken(TTokenKind.RParen)
		
		Return New TCallableTypeSuffixSyntax(lparen, parameterDeclaration, rparen)
	End Method
	
	Method ParseVariableDeclaratorList:TVariableDeclaratorListSyntax(initializersOption:EInitializersOption) ' always succeeds
		Local elements:TVariableDeclaratorListElementSyntax[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local declarator:TVariableDeclaratorSyntax = ParseVariableDeclarator(initializersOption)
			If Not declarator Then
				ReportError "Expected variable name"
				' TODO: fix dropped trailing comma
				Exit
			End If
			elements :+ [New TVariableDeclaratorListElementSyntax(comma, declarator)]
		Forever
		Return New TVariableDeclaratorListSyntax(elements)
	End Method
	
	Method ParseParenExpressionList:TParenExpressionListSyntax(allowEmptyElements:Int)
		Local lparen:TSyntaxToken = TryTakeToken(TTokenKind.LParen)
		If Not lparen Then Return Null
		
		Local list:TExpressionListSyntax = ParseExpressionList(allowEmptyElements)
		
		Local rparen:TSyntaxToken = TakeToken(TTokenKind.RParen)
		
		Return New TParenExpressionListSyntax(lparen, list, rparen)
	End Method
	
	Method ParseBracketExpressionList:TBracketExpressionListSyntax(allowEmptyElements:Int)
		Local lbracket:TSyntaxToken = TryTakeToken(TTokenKind.LBracket)
		If Not lbracket Then Return Null
		
		Local list:TExpressionListSyntax = ParseExpressionList(allowEmptyElements)
		
		Local rbracket:TSyntaxToken = TakeToken(TTokenKind.RBracket)
		
		Return New TBracketExpressionListSyntax(lbracket, list, rbracket)
	End Method
	
	Method ParseExpressionList:TExpressionListSyntax(allowEmptyElements:Int) ' always succeeds
		Local elements:TExpressionListElementSyntax[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local expression:IExpressionSyntax = ParseExpression()
			If Not expression Then
				If Not elements And currentToken.Kind() <> TTokenKind.Comma Then Exit ' empty list
				'If Not allowEmptyElements Or currentToken.Kind() <> TTokenKind.Comma Then ReportError "Expected expression"; Exit
				If Not allowEmptyElements Then
					ReportError "Expected expression"
					If currentToken.Kind() <> TTokenKind.Comma Then
						elements :+ [New TExpressionListElementSyntax(comma, GenerateMissingExpression())]
						' TODO: fix dropped trailing comma
						Exit ' TODO: try to skip ahead to next comma if ParseExpression fails?
					End If
				End If
			End If
			elements :+ [New TExpressionListElementSyntax(comma, expression)]
		Forever
		Return New TExpressionListSyntax(elements)
	End Method
	
	Method ParseTypeList:TTypeListSyntax() ' always succeeds
		Local elements:TTypeListElementSyntax[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local type_:TTypeSyntax = ParseType(False)
			If Not type_ Then
				ReportError "Expected type"
				' TODO: fix dropped trailing comma
				Exit
			End If
			elements :+ [New TTypeListElementSyntax(comma, type_)]
		Forever
		Return New TTypeListSyntax(elements)
	End Method
	
	Method ParseQualifiedNameList:TQualifiedNameListSyntax() ' always succeeds
		Local elements:TQualifiedNameListElementSyntax[]
		Repeat
			Local comma:TSyntaxToken
			If elements Then
				comma = TryTakeToken(TTokenKind.Comma)
				If Not comma Then Exit
			End If
			Local name:TQualifiedNameSyntax = ParseQualifiedName()
			If Not name Then
				ReportError "Expected identifier"
				' TODO: fix dropped trailing comma
				Exit
			End If
			elements :+ [New TQualifiedNameListElementSyntax(comma, name)]
		Forever
		Return New TQualifiedNameListSyntax(elements)
	End Method
	
	Method ParseMetaData:TMetaDataSyntax()
		Local lbrace:TSyntaxToken = TryTakeToken(TTokenKind.LBrace)
		If Not lbrace Then Return Null
		
		Local elements:TMetaDataElementSyntax[]
		Repeat
			Local key:TNameSyntax = ParseName()
			If Not key Then Exit
			Local eq:TSyntaxToken = TryTakeToken(TTokenKind.Eq)
			Local value:IExpressionSyntax
			If eq Then
				value = ParseConstantExpression()
				If Not value Then
					ReportError "Expected constant value"
					value = New TStringLiteralExpressionSyntax(GenerateMissingToken(TTokenKind.StringLiteral, ""), Null)
				End If
			End If
			elements :+ [New TMetaDataElementSyntax(key, eq, value)]
		Forever
		
		Local rbrace:TSyntaxToken = TakeToken(TTokenKind.RBrace)
		
		Return New TMetaDataSyntax(lbrace, elements, rbrace)
	End Method
	
	Method ParseName:TNameSyntax()
		Local identifier:TSyntaxToken = TryTakeToken(TTokenKind.Identifier)
		If Not identifier Then Return Null
		
		Return New TNameSyntax(identifier)
	End Method
	
	Method ParseQualifiedName:TQualifiedNameSyntax()
		Local parts:TQualifiedNamePartSyntax[]
		Repeat
			Local dot:TSyntaxToken = TryTakeToken(TTokenKind.Dot)
			If parts And Not dot Then Exit
			Local identifier:TSyntaxToken = TryTakeToken(TTokenKind.Identifier)
			If Not identifier Then
				If Not parts Then Return Null Else ReportError "Expected identifier"; Exit
			End If
			parts :+ [New TQualifiedNamePartSyntax(dot, identifier)]
		Forever
		Return New TQualifiedNameSyntax(parts)
	End Method
	
	Method ParseOperator:TOperatorSyntax(tokenKinds:TTokenKind[])
		Assert tokenKinds Else "Operator must consist of at least one token"
		Local tokens:TSyntaxToken[tokenKinds.length]
		For Local t:Int = 0 Until tokenKinds.length
			tokens[t] = TryTakeToken(tokenKinds[t])
			If Not tokens[t] Then Return Null
			If t <> 0 Then
				' do not allow any trivia inbetween the tokens that make up the operator
				If tokens[t - 1].trailingTrivia Or tokens[t].leadingTrivia Then Return Null
			End If
		Next
		Return New TOperatorSyntax(tokens)
	End Method
	
	Method ParseContextualKeyword:TContextualKeywordSyntax(canonicalValue:String)
		' parses an identifier that has canonicalValue as its value (by case-insensitive comparison)
		If currentToken.Kind() = TTokenKind.Identifier And currentToken.lexerToken.value.ToLower() = canonicalValue.ToLower() Then
			Local identifier:TSyntaxToken = TakeToken(TTokenKind.Identifier)
			Return New TContextualKeywordSyntax(canonicalValue, identifier)
		Else
			Return Null
		End If
	End Method
	
	Method ParseAssignment:TAssignmentSyntax(constantOnly:Int = False)
		Local eq:TSyntaxToken = TryTakeToken(TTokenKind.Eq)
		If Not eq Then Return Null
		
		Local expression:IExpressionSyntax
		If constantOnly Then
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
		
		Return New TAssignmentSyntax(eq, expression)
	End Method
	
	Method ParseConstantAssignment:TAssignmentSyntax()
		Return ParseAssignment(True)
	End Method
	
	Method ParseStatementSeparator:TStatementSeparatorSyntax() ' parses a logical newline (semicolons or non-escaped line break)
		Local token:TSyntaxToken = TryTakeToken([TTokenKind.Linebreak, TTokenKind.Semicolon])
		If Not token Then Return Null
		
		Return New TStatementSeparatorSyntax(token)
	End Method
	
	Method ParseStatementSeparators:TStatementSeparatorSyntax[]()
		Local separators:TStatementSeparatorSyntax[]
		Repeat
			Local separator:TStatementSeparatorSyntax = ParseStatementSeparator()
			If separator Then separators :+ [separator] Else Exit
		Forever
		Return separators
	End Method
	
End Type
