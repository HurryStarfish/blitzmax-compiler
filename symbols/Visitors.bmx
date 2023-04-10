SuperStrict
Import "Scope.bmx"
Import "../syntax/SyntaxVisitor.bmx"
Import BRL.LinkedList
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

Function GetScope:TScope(link:TSyntaxLink, scopes:TMap)'<TSyntaxLink, TScope>
	' gets the scope for this link, or, if none present, the enclosing scope
	Repeat
		Local scope:TScope = TScope(scopes[link])
		If scope Then Return scope
		link = link.GetParent()
		If Not link Then Return Null
	Forever
End Function

Public
Type TCreateScopesVisitor Extends TSyntaxVisitor Final
	Field ReadOnly scopes:TMap = New TMap'<TSyntaxLink, TScope>
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TCodeBlockSyntax)
		Local scope:TScope = TScope(scopes[link])
		Local parentSyntax:ISyntax = link.GetParentSyntax()
		Local inheritsLocals:Int = IStatementSyntax(parentSyntax) Or ..
		                           TIfBranchSyntax(parentSyntax) Or ..
		                           TSelectBranchSyntax(parentSyntax) Or ..
		                           TTryBranchSyntax(parentSyntax) Or ..
		                           TIncludeDirectiveSyntax(link.GetParent().GetParentSyntax()) And TIncludeDirectiveSyntax(link.GetParent().GetParentSyntax()).body.block = syntax
		scope = New TScope(GetScope(link.GetParent(), scopes), inheritsLocals)
		scopes[link] = scope
		' TODO
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TEnumDeclarationSyntax)
		scopes[link] = New TScope(GetScope(link.GetParent(), scopes), False)
	End Method
End Type

Type TCollectTypeDeclarationSyntaxesVisitor Extends TSyntaxVisitor Final
	' type declarations refer to each other; creating a declaration requires the declarations
	' for all its super/base types to be created first
	' these dependencies can span across compilation units, can span across scopes even within the same
	' compilation unit (regardless of nesting structure), and are independent of code order
	' restriction: imports can not have cycles, which means two types from different compilation units
	'              cannot both depend on a type from the other compilation unit
	' restriction: type dependencies can not have cycles, so two types cannot both depend on each other
	' restriction: types cannot depend on their own nested types (outer types count as dependencies)
	' this means:
	' - first, info about all type declarations (and their dependencies) within the compilation unit
	'   must be gathered
	' - type declarations (assuming they conform to the above restrictions) can then be created in
	'   some specific order
	'   - resolving dependencies requires being able to resolve fully or partially qualified names
	' - if any types do not conform to the above restriction, an error must be reported and the
	'   problematic dependencies be dropped in order to recover
	Field ReadOnly typeDeclarationSyntaxLinks:TList = New TList'<TSyntaxLink<TTypeDeclarationSyntax>>
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TTypeDeclarationSyntax)
		typeDeclarationSyntaxLinks.AddLast link
	End Method
End Type

Type TCreateScopesAndInsertDeclarationsVisitor Extends TSyntaxVisitor Final
	Field ReadOnly scopes:TMap = New TMap'<TSyntaxLink, TScope>
	
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
			Local bodyScope:TScope = CreateAndAttachScope(link.FindChild(syntax.body), syntax.body, False)
			AddSymbolsForDeclaratorsToScope callableTypeSuffixSyntax.parameterDeclaration, bodyScope, False, EAllowedDeclaratorCount.Any
		End If
		
		' TODO: handle type parameters
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TForStatementSyntax)
		' counter variable declaration
		If TForCounterDeclarationSyntax(syntax.counter) Then
			Local declaration:TVariableDeclarationSyntax = TForCounterDeclarationSyntax(syntax.counter).declaration
			Local bodyScope:TScope = CreateAndAttachScope(link.FindChild(syntax.body), syntax.body, False)
			AddSymbolsForDeclaratorsToScope declaration, bodyScope, True, EAllowedDeclaratorCount.ExactlyOne
		End If
	End Method
	
	Method VisitTopDown(link:TSyntaxLink, syntax:TCatchTryBranchSyntax)
		' exception variable declaration
		Local declaration:TVariableDeclarationSyntax = syntax.declaration
		Local bodyScope:TScope = CreateAndAttachScope(link.FindChild(syntax.body), syntax.body, False)
		AddSymbolsForDeclaratorsToScope declaration, bodyScope, False, EAllowedDeclaratorCount.ExactlyOne
	End Method
	
	Private
	Method GetParentCodeBlockScope:TScope(link:TSyntaxLink) ' requires the scope to exist already
		Assert TCodeBlockSyntax(link.GetParent().GetSyntaxOrSyntaxToken()) Else "Declaration is not in a block"
		Local scope:TScope = TScope(scopes[link.GetParent()])
		Assert scope Else "Missing scope"
		Return scope
	End Method
	
	Method CreateAndAttachScope:TScope(link:TSyntaxLink, syntax:TCodeBlockSyntax, inheritsLocalsFromParent:Int) ' requires the scope to not exist yet
		Assert link.GetSyntaxOrSyntaxToken() = syntax Else "Link does not match syntax"
		Assert Not scopes.Contains(link) Else "Code block unexpectedly already has a scope"
		Local scope:TScope = New TScope(GetScope(link.GetParent(), scopes), inheritsLocalsFromParent)
		scopes[link] = scope
		Return scope
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



Function CreateTypeDeclarations(typeDeclarationSyntaxLinks:TList, scopes:TMap)'<TTypeDeclaration>'<TSyntaxLink<TTypeDeclarationSyntax>>'<TSyntaxLink, TScope>
	' creates declarations and inserts them into scopes
	Rem
	-> place in queue, try to resolve in arbitrary number of passes
	   in each pass, skip where any dependencies are unresolved
	   if pass without progress, detect cycles, error on involved types and recover by dropping dependencies
	End Rem
	Function Name:String(link:TSyntaxLink)
		Local syntax:TTypeDeclarationSyntax = TTypeDeclarationSyntax(link.GetSyntaxOrSyntaxToken())
		If TClassDeclarationSyntax(syntax) Then
			Return TClassDeclarationSyntax(syntax).name.identifier.lexerToken.value
		Else If TStructDeclarationSyntax(syntax) Then
			Return TStructDeclarationSyntax(syntax).name.identifier.lexerToken.value
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Return TInterfaceDeclarationSyntax(syntax).name.identifier.lexerToken.value
		Else If TEnumDeclarationSyntax(syntax) Then
			Return TEnumDeclarationSyntax(syntax).name.identifier.lexerToken.value
		Else
			RuntimeError "Missing case"
		End If
	End Function
	
	Local declarations:TList = New TList'<TTypeDeclaration>
	Local resolutionQueue:TList = New TList'<TSyntaxLink<TTypeDeclarationSyntax>>
	
	For Local link:TSyntaxLink = EachIn typeDeclarationSyntaxLinks
		Local syntax:TTypeDeclarationSyntax = TTypeDeclarationSyntax(link.GetSyntaxOrSyntaxToken())
		' put types without dependencies at the front of the queue
		If HasAnyExplicitDependencies(syntax) Then resolutionQueue.AddLast link Else resolutionQueue.AddFirst link
	Next
	
	Local passes:Int = 0
	Repeat
		Local successes:Int = 0
		Local queueLink:TLink = resolutionQueue.FirstLink()
		While queueLink
			Local link:TSyntaxLink = TSyntaxLink(queueLink.Value())
			Local declaration:TTypeDeclaration = TryCreateDeclaration(link, declarations, scopes)
			If declaration Then
				successes :+ 1
				declarations.AddLast declaration
				GetScope(link, scopes).AddDeclaration declaration
				queueLink.Remove
			End If
			queueLink = queueLink.NextLink()
		Wend
		passes :+ 1
		If resolutionQueue.IsEmpty() Then
			Exit
		Else If successes = 0 Then
			Local a:Object[] = declarations.ToArray()
			Local q:Object[] = resolutionQueue.ToArray()
			RuntimeError "TODO: error: missing or circular dependency"
			For Local link:TSyntaxLink = EachIn resolutionQueue
				'Print "~t" + Name(link)
			Next
			' TODO
		End If
	Forever
	
	Function TryCreateDeclaration:TTypeDeclaration(link:TSyntaxLink, existingDeclarations:TList, scopes:TMap)'<TTypeDeclarationSyntax>'<TTypeDeclaration>'<TSyntaxLink, TScope>
		Local syntax:TTypeDeclarationSyntax = TTypeDeclarationSyntax(link.GetSyntaxOrSyntaxToken())
		Assert syntax Else "Wrong syntax link type"
		If TClassDeclarationSyntax(syntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(syntax)
			
			Local superClassDeclaration:TClassDeclaration
			If syntax.superClass Then
				Local dependencyDeclaration:TTypeDeclaration = ResolveDependency(GetScope(link, scopes), syntax.superClass, existingDeclarations)
				If dependencyDeclaration Then
					superClassDeclaration = TClassDeclaration(dependencyDeclaration)
					If Not superClassDeclaration Then
						RuntimeError "TODO: error: wrong kind of type"
					End If
				Else
					Return Null ' cannot resolve (yet?)
				End If
			End If
			
			Local superInterfaceDeclarations:TInterfaceDeclaration[]
			If syntax.superInterfaces Then
				superInterfaceDeclarations = New TInterfaceDeclaration[syntax.superInterfaces.elements.length]
				For Local e:Int = 0 Until syntax.superInterfaces.elements.length
					Local superInterfaceSyntax:TTypeSyntax = syntax.superInterfaces.elements[e].type_
					If superInterfaceSyntax Then
						Local dependencyDeclaration:TTypeDeclaration = ResolveDependency(GetScope(link, scopes), superInterfaceSyntax, existingDeclarations)
						If dependencyDeclaration Then
							Local superInterfaceDeclaration:TInterfaceDeclaration = TInterfaceDeclaration(dependencyDeclaration)
							If superInterfaceDeclaration Then
								superInterfaceDeclarations[e] = superInterfaceDeclaration
							Else
								RuntimeError "TODO: error: wrong kind of type"
							End If
						Else
							Return Null ' cannot resolve (yet?)
						End If
					End If
				Next
			End If
			
			Assert Not syntax.typeParameters Else "TODO"
			
			Local superClass:TClass
			Local superInterfaces:TInterface[]
			If superClassDeclaration Then superClass = New TClass(syntax.superClass, superClassDeclaration)
			For Local i:Int = 0 Until superInterfaceDeclarations.length
				If superInterfaceDeclarations[i] Then superInterfaces :+ [New TInterface(syntax.superInterfaces.elements[i].type_, superInterfaceDeclarations[i])]
			Next
			Local declaration:TClassDeclaration = New TClassDeclaration(syntax, superClass, superInterfaces)
			Return declaration
		Else If TStructDeclarationSyntax(syntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(syntax)
			
			Assert Not syntax.typeParameters Else "TODO"
			
			Local declaration:TStructDeclaration = New TStructDeclaration(syntax)
			Return declaration
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(syntax)
			
			Local superInterfaceDeclarations:TInterfaceDeclaration[]
			If syntax.superInterfaces Then
				superInterfaceDeclarations = New TInterfaceDeclaration[syntax.superInterfaces.elements.length]
				For Local e:Int = 0 Until syntax.superInterfaces.elements.length
					Local superInterfaceSyntax:TTypeSyntax = syntax.superInterfaces.elements[e].type_
					If superInterfaceSyntax Then
						Local dependencyDeclaration:TTypeDeclaration = ResolveDependency(GetScope(link, scopes), superInterfaceSyntax, existingDeclarations)
						If dependencyDeclaration Then
							Local superInterfaceDeclaration:TInterfaceDeclaration = TInterfaceDeclaration(dependencyDeclaration)
							If superInterfaceDeclaration Then
								superInterfaceDeclarations[e] = superInterfaceDeclaration
							Else
								RuntimeError "TODO: error: wrong kind of type"
							End If
						Else
							Return Null ' cannot resolve (yet?)
						End If
					End If
				Next
			End If
			
			Assert Not syntax.typeParameters Else "TODO"
			
			Local superInterfaces:TInterface[]
			For Local i:Int = 0 Until superInterfaceDeclarations.length
				If superInterfaceDeclarations[i] Then superInterfaces :+ [New TInterface(syntax.superInterfaces.elements[i].type_, superInterfaceDeclarations[i])]
			Next
			Local declaration:TInterfaceDeclaration = New TInterfaceDeclaration(syntax, superInterfaces)
			Return declaration
		Else If TEnumDeclarationSyntax(syntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax)
			
			Local baseTypeDeclaration:TStructDeclaration
			If syntax.baseType Then
				Local dependencyDeclaration:TTypeDeclaration = ResolveDependency(GetScope(link, scopes), syntax.baseType, existingDeclarations)
				If dependencyDeclaration Then
					If syntax.baseType Then
						baseTypeDeclaration = TStructDeclaration(dependencyDeclaration)
						If Not baseTypeDeclaration Then
							RuntimeError "TODO: error: wrong kind of type"
						End If
					End If
				Else
					Return Null ' cannot resolve (yet?)
				End If
			End If
			
			Local baseType:TStruct
			If baseTypeDeclaration Then baseType = New TStruct(syntax.baseType, baseTypeDeclaration)
			Local hasFlags:Int = syntax.flagsKeyword <> Null
			Local declaration:TEnumDeclaration = New TEnumDeclaration(syntax, baseType, hasFlags)
			Return declaration
		Else
			RuntimeError "Missing case"
		End If
		
		Function ResolveDependency:TTypeDeclaration(lookupScope:TScope, dependencySyntax:TTypeSyntax, existingDeclarations:TList)'<TTypeDeclaration>
			If Not dependencySyntax.base Then RuntimeError "TODO: error"
			If dependencySyntax.marshallingModifier Then RuntimeError "TODO: error"
			Assert Not dependencySyntax.typeArguments Else "TODO"
			If dependencySyntax.suffixes Then RuntimeError "TODO: error"
			
			'TODO: proper lookup, this is just for testing
			Local n:TQualifiedNameSyntax = TQualifiedNameTypeBaseSyntax(dependencySyntax.base).name
			Local d:IDeclaration[] = lookupScope.LookUpDeclarationWIP(n.parts[n.parts.length - 1].identifier.lexerToken.value.ToLower(), False)
			If Not d Then
				Return Null
			Else If d.length > 1 Then
				RuntimeError "TODO: error: ambiguous"
			Else If TTypeDeclaration(d[0]) Then
				Return TTypeDeclaration(d[0])
			Else
				RuntimeError "TODO: error: not a type declaration"
			End If
		End Function
	End Function
	
	Function HasAnyExplicitDependencies:Int(syntax:TTypeDeclarationSyntax)
		' TODO: type parameters
		If TClassDeclarationSyntax(syntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			Return syntax.superClass Or syntax.superInterfaces
		Else If TStructDeclarationSyntax(syntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			Return False
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			Return syntax.superInterfaces <> Null
		Else If TEnumDeclarationSyntax(syntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax)
			Return syntax.baseType <> Null
		Else
			RuntimeError "Missing case"
		End If
	End Function
	
	Rem
	Function ExplicitDependencies:TTypeSyntax[](syntax:TTypeDeclarationSyntax)
		' does not include Object as an implicit super type or implicit base types of enums
		' since those are declared in BRL.Blitz and guaranteed to already be available
		' TODO: type parameters
		Local dependencies:TTypeSyntax[]
		If TClassDeclarationSyntax(syntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			If syntax.superInterfaces Then
				For Local e:TTypeListElementSyntax = EachIn syntax.superInterfaces.elements
					If e.type_ Then dependencies :+ [e.type_]
				Next
			End If
			If syntax.superClass Then dependencies = [syntax.superClass] + dependencies
		Else If TStructDeclarationSyntax(syntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			If syntax.superInterfaces Then
				For Local e:TTypeListElementSyntax = EachIn syntax.superInterfaces.elements
					If e.type_ Then dependencies :+ [e.type_]
				Next
			End If
		Else If TEnumDeclarationSyntax(syntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax)
			If syntax.baseType Then dependencies = [syntax.baseType]
		Else
			RuntimeError "Missing case"
		End If
		Return dependencies
	End Function
	End Rem
End Function
