SuperStrict
Import "Scope.bmx"
Import "../syntax/SyntaxVisitor.bmx"
Import BRL.Map



Private
'Enum EVisibility
'	Public_
'	Protected_
'	Private_
'End Enum

Enum EAllowedDeclaratorCount
	ExactlyOne
	AtLeastOne
	Any
End Enum

Public
Type TCreateScopesAndInsertNamedDeclarationsVisitor Extends TSyntaxVisitor
	Field ReadOnly scopes:TMap = New TMap'<TSyntaxLink, TScope>
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TCodeBlockSyntax)
		' create scope for code block if it hasn't already been given one by another method
		Local scope:TScope = TScope(scopes[link])
		If Not scope Then
			scope = New TScope(FindParentScope(link))
			scopes[link] = scope
		End If
		
		'Local currentVisibility:EVisibility = EVisibility.Public_
		For Local elementSyntax:ICodeBlockElementSyntax = EachIn syntax.elements
			'If TVisibilityDirectiveSyntax(elementSyntax) Then
			'	Select TVisibilityDirectiveSyntax.visibility.Kind()
			'		Case TTokenKind.Public_    currentVisibility:EVisibility = EVisibility.Public_
			'		Case TTokenKind.Protected_ currentVisibility:EVisibility = EVisibility.Protected_
			'		Case TTokenKind.Private_   currentVisibility:EVisibility = EVisibility.Private_
			'		Default RuntimeError "Missing case"
			'	End Select
			'End If
			If TTypeDeclarationSyntax(elementSyntax) Then
				Local typeDeclarationSyntax:TTypeDeclarationSyntax = TTypeDeclarationSyntax(elementSyntax)
				Local typeDeclarationSyntaxLink:TSyntaxLink = link.FindChild(typeDeclarationSyntax)
				Local typeDeclaration:TTypeDeclaration
				' TODO: handle type parameters
				If TClassDeclarationSyntax(typeDeclarationSyntax) Then
					Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(typeDeclarationSyntax)
					Local name:String = syntax.name.identifier.lexerToken.value
					typeDeclaration = New TClassDeclaration(name, Null, Null) ' TODO
				Else If TStructDeclarationSyntax(typeDeclarationSyntax) Then
					Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(typeDeclarationSyntax)
					Local name:String = syntax.name.identifier.lexerToken.value
					typeDeclaration = New TStructDeclaration(name)
				Else If TInterfaceDeclarationSyntax(typeDeclarationSyntax) Then
					Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(typeDeclarationSyntax)
					Local name:String = syntax.name.identifier.lexerToken.value
					typeDeclaration = New TInterfaceDeclaration(name, Null) ' TODO
				Else If TEnumDeclarationSyntax(typeDeclarationSyntax) Then
					Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(typeDeclarationSyntax)
					Local name:String = syntax.name.identifier.lexerToken.value
					typeDeclaration = New TEnumDeclaration(name, Null, syntax.flagsKeyword <> Null) ' TODO
				Else
					RuntimeError "Missing case"
				End If
				scope.AddDeclaration typeDeclaration
			End If
		Next
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TVariableDeclarationSyntax)
		' all cases of declarations that aren't direct children of a code block
		' (callable parameters, For loop counters, Catch exception variables) should
		' be handled by their parent statements; since these are top-down visits, that
		' means they have already been handled when we get here
		If Not TCodeBlockSyntax(link.GetParent().GetSyntaxOrSyntaxToken()) Then Return
		
		Local scope:TScope = GetParentCodeBlockScope(link)
		' declarations without a declaration keyword are never direct children of a code block
		AddSymbolsForDeclaratorsToScope syntax, scope, True, EAllowedDeclaratorCount.AtLeastOne
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TEnumDeclarationSyntax)
		Assert Not scopes.Contains(syntax) Else "Enum declaration unexpectedly already has a scope"
		scopes[link] = New TScope(FindParentScope(link))
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TEnumMemberDeclarationSyntax)
		Local l:TSyntaxLink = link.GetParent()
		Local e:TEnumDeclarationSyntax = TEnumDeclarationSyntax(l.GetSyntaxOrSyntaxToken())
		Assert e Else "Parent of enum member is not an enum declaration"
		Local nameStr:String = syntax.name.identifier.lexerToken.value
		Local scope:TScope = TScope(scopes[l])
		scope.AddDeclaration New TConstDeclaration(nameStr)
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TCallableDeclarationSyntax)
		Local scope:TScope = GetParentCodeBlockScope(link)
		Local nameStr:String
		If syntax.name.identifierName Then
			Local nameStr:String = syntax.name.identifierName.identifier.lexerToken.value
			Local declaration:TCallableDeclaration
			Select syntax.initiatorKeyword.Kind()
				Case TTokenKind.Function_ declaration = New TFunctionDeclaration(nameStr)
				Case TTokenKind.Method_ declaration = New TMethodDeclaration(nameStr)
				Default RuntimeError "Missing case"
			End Select
			scope.AddDeclaration declaration
		Else If syntax.name.keywordName Then
			Local declaration:TCallableDeclaration
			Select syntax.name.keywordName.Kind()
				Case TTokenKind.New_ declaration = New TConstructorDeclaration(nameStr)
				Case TTokenKind.Delete_ declaration = New TFinalizerDeclaration(nameStr)
				Default RuntimeError "Missing case"
			End Select
			scope.AddDeclaration declaration
		Else If syntax.name.operatorName Then
			'TOperatorSyntax
			Throw "TODO"
		Else
			RuntimeError "Missing case"
		End If
		
		If syntax.body Then
			' parameter variable declarations
			Assert syntax.type_.suffixes.length >= 1 Else "Callable declaration is missing type suffixes"
			Local callableTypeSuffixSyntax:TCallableTypeSuffixSyntax = TCallableTypeSuffixSyntax(syntax.type_.suffixes[syntax.type_.suffixes.length - 1])
			If Not callableTypeSuffixSyntax Then ' parameter list was missing in code
				Throw "TODO" ' compile error
				Return
			End If
			Local bodyScope:TScope = CreateAndAttachScope(link.FindChild(syntax.body), syntax.body)
			AddSymbolsForDeclaratorsToScope callableTypeSuffixSyntax.parameterDeclaration, bodyScope, False, EAllowedDeclaratorCount.Any
		End If
		
		' TODO: handle type parameters
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TForStatementSyntax)
		' counter variable declaration
		If TForCounterDeclarationSyntax(syntax.counter) Then
			Local declaration:TVariableDeclarationSyntax = TForCounterDeclarationSyntax(syntax.counter).declaration
			Local bodyScope:TScope = CreateAndAttachScope(link.FindChild(syntax.body), syntax.body)
			AddSymbolsForDeclaratorsToScope declaration, bodyScope, True, EAllowedDeclaratorCount.ExactlyOne
		End If
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TCatchTryBranchSyntax)
		' exception variable declaration
		Local declaration:TVariableDeclarationSyntax = syntax.declaration
		Local bodyScope:TScope = CreateAndAttachScope(link.FindChild(syntax.body), syntax.body)
		AddSymbolsForDeclaratorsToScope declaration, bodyScope, False, EAllowedDeclaratorCount.ExactlyOne
	End Method
	
	Private
	Method GetParentCodeBlockScope:TScope(link:TSyntaxLink) ' requires the scope to exist already
		Assert TCodeBlockSyntax(link.GetParent().GetSyntaxOrSyntaxToken()) Else "Declaration is not in a block"
		Local scope:TScope = TScope(scopes[link.GetParent()])
		Assert scope Else "Missing scope"
		Return scope
	End Method
	
	Method CreateAndAttachScope:TScope(link:TSyntaxLink, syntax:TCodeBlockSyntax) ' requires the scope to not exist yet
		Assert link.GetSyntaxOrSyntaxToken() = syntax Else "Link does not match syntax"
		Assert Not scopes.Contains(link) Else "Code block unexpectedly already has a scope"
		Local scope:TScope = New TScope(FindParentScope(link))
		scopes[link] = scope
		Return scope
	End Method
	
	Method FindParentScope:TScope(link:TSyntaxLink)
		Repeat
			link = link.GetParent()
			If Not link Then Return Null
			Local scope:TScope = TScope(scopes[link])
			If scope Then Return scope
		Forever
	End Method
	
	Function AddSymbolsForDeclaratorsToScope(declaration:TVariableDeclarationSyntax, scope:TScope, shouldHaveDeclarationKeywords:Int, allowedDeclaratorCount:EAllowedDeclaratorCount)
		Local declarationKeyword:TSyntaxToken = declaration.declarationKeyword
		If Not declarationKeyword And shouldHaveDeclarationKeywords Then
			Throw "TODO"
			Return
		Else If declarationKeyword And Not shouldHaveDeclarationKeywords Then
			Throw "TODO"
			' only report error but continue
		End If
		
		' TODO: allow/disallow readonly
		
		Select allowedDeclaratorCount
			Case EAllowedDeclaratorCount.ExactlyOne
				If declaration.declarators.elements.length < 1 Then
					Throw "TODO"
				Else If declaration.declarators.elements.length > 1 Then
					Throw "TODO" ' this should haveve been handled by the parser already
					ProcessDeclarator declaration.declarators.elements[0], scope, declarationKeyword, shouldHaveDeclarationKeywords
				Else
					ProcessDeclarator declaration.declarators.elements[0], scope, declarationKeyword, shouldHaveDeclarationKeywords
				End If
			Case EAllowedDeclaratorCount.AtLeastOne
				If declaration.declarators.elements.length >= 1 Then
					For Local d:Int = 0 Until declaration.declarators.elements.length
						ProcessDeclarator declaration.declarators.elements[d], scope, declarationKeyword, shouldHaveDeclarationKeywords
					Next
				Else
					Throw "TODO"
				End If
			Case EAllowedDeclaratorCount.Any
				For Local d:Int = 0 Until declaration.declarators.elements.length
					ProcessDeclarator declaration.declarators.elements[d], scope, declarationKeyword, shouldHaveDeclarationKeywords
				Next
			Default RuntimeError "Missing case"
		End Select
		
		Function ProcessDeclarator(syntax:TVariableDeclaratorListElementSyntax, scope:TScope, declarationKeyword:TSyntaxToken, shouldHaveDeclarationKeyword:Int)
			Local nameStr:String = syntax.declarator.name.identifier.lexerToken.value
			
			Local symbol:TVariableDeclaration
			If (Not declarationKeyword) Or (Not shouldHaveDeclarationKeyword) Then
				'                                   ^ ignore keyword if unexpected
				symbol = New TLocalDeclaration(nameStr)
			Else
				Select declarationKeyword.Kind()
					Case TTokenKind.Const_  symbol = New TConstDeclaration(nameStr)
					Case TTokenKind.Global_ symbol = New TGlobalDeclaration(nameStr)
					Case TTokenKind.Local_  symbol = New TLocalDeclaration(nameStr)
					Case TTokenKind.Field_  symbol = New TFieldDeclaration(nameStr)
					Default RuntimeError "Missing case"
				End Select
			End If
			'If scope.GetSymbolForName(nameStr) Then Throw "TODO" ' TODO: handle duplicate declarations
			scope.AddDeclaration symbol
		End Function
	End Function
End Type
