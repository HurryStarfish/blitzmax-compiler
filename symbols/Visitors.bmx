SuperStrict
Import "Scope.bmx"
Import "../syntax/SyntaxVisitor.bmx"
Import "../util/GraphUtils.bmx"
Import BRL.LinkedList
Import BRL.Map
Import brl.standardio ' TODO: remove again


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
	
	Function GetExplicitDependencies:TSyntaxLink[](link:TSyntaxLink, syntax:TTypeDeclarationSyntax)'<TTypeSyntax | TTypeDeclarationSyntax>
		' does not include Object as an implicit super type or implicit base types of enums
		' those are declared in BRL.Blitz and guaranteed to already be available
		Local dependencies:TSyntaxLink[]'<TTypeSyntax | TTypeDeclarationSyntax>[]
		
		' outer type
		If TCodeBlockSyntax(link.GetParentSyntax()) And TTypeDeclarationSyntax(link.GetParent().GetParentSyntax()) Then
			dependencies :+ [link.GetParent().GetParent()]
		Else If TTypeDeclarationSyntax(link.GetParentSyntax()) Then
			dependencies :+ [link.GetParent()]
		End If
		
		' TODO: type parameters
		
		' super/base types
		If TClassDeclarationSyntax(syntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			If syntax.superClass Then dependencies :+ [link.FindChild(syntax.superClass)]
			If syntax.superInterfaces Then
				Local superInterfacesLink:TSyntaxLink = link.FindChild(syntax.superInterfaces)
				For Local e:TTypeListElementSyntax = EachIn syntax.superInterfaces.elements
					If e.type_ Then dependencies :+ [superInterfacesLink.FindChild(e).FindChild(e.type_)]
				Next
			End If
		Else If TStructDeclarationSyntax(syntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(syntax)
			Assert Not syntax.typeParameters Else "TODO"
			If syntax.superInterfaces Then
				Local superInterfacesLink:TSyntaxLink = link.FindChild(syntax.superInterfaces)
				For Local e:TTypeListElementSyntax = EachIn syntax.superInterfaces.elements
					If e.type_ Then dependencies :+ [superInterfacesLink.FindChild(e).FindChild(e.type_)]
				Next
			End If
		Else If TEnumDeclarationSyntax(syntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax)
			If syntax.baseType Then dependencies :+ [link.FindChild(syntax.baseType)]
		Else
			RuntimeError "Missing case"
		End If
		Return dependencies
	End Function
	
	Function GetParentCodeBlockScope:TScope(link:TSyntaxLink, scopes:TMap)
		Assert TCodeBlockSyntax(link.GetParent().GetSyntaxOrSyntaxToken()) Else "Declaration is not in a block"
		Local scope:TScope = TScope(scopes[link.GetParent()])
		Assert scope Else "Missing scope"
		Return scope
	End Function
	
	Function GetName:String(link:TSyntaxLink)
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
	
	Type TUnresolvedTypeDeclaration Extends TTypeDeclaration Final
		Field ReadOnly link:TSyntaxLink
		Method New(name:String, link:TSyntaxLink)
			Self.name = name
			Self.link = link
		End Method
		Method GetSyntax:TTypeDeclarationSyntax() Override
			RuntimeError "no"
		End Method
	End Type
	
	'Local resolutionQueue:TList = New TList'<TSyntaxLink<TTypeDeclarationSyntax>>
	
	' create list of unresolved declarations and insert into scopes if possible
	Local typeDeclarations:TList = New TList'<TTypeDeclaration>
	For Local link:TSyntaxLink = EachIn typeDeclarationSyntaxLinks
		Local syntax:TTypeDeclarationSyntax = TTypeDeclarationSyntax(link.GetSyntaxOrSyntaxToken())
		Local unresolvedDeclaration:TUnresolvedTypeDeclaration = New TUnresolvedTypeDeclaration(GetName(link), link)
		typeDeclarations.AddLast unresolvedDeclaration
		GetParentCodeBlockScope(link, scopes).AddDeclaration unresolvedDeclaration
		' TODO: handle failure to add (duplicate declaration)
	Next
	
	' create dependency graph
	Local graphNodes:TList = New TList'<TGraphNode>
	Local graphNodeMap:TMap = New TMap'<TSyntaxLink, TGraphNode>
	Local typeDeclarationListLink:TLink = typeDeclarations.FirstLink()
	For Local link:TSyntaxLink = EachIn typeDeclarationSyntaxLinks
		Local node:TGraphNode = New TGraphNode(typeDeclarationListLink.Value())
		graphNodes.AddLast node
		graphNodeMap[link] = node
		typeDeclarationListLink = typeDeclarationListLink.NextLink()
	Next
	For Local link:TSyntaxLink = EachIn typeDeclarationSyntaxLinks
		Local syntax:TTypeDeclarationSyntax = TTypeDeclarationSyntax(link.GetSyntaxOrSyntaxToken())
		Local explicitDependencyLinks:TSyntaxLink[] = GetExplicitDependencies(link, syntax)
		Local edges:TGraphNode[explicitDependencyLinks.length]
		For Local d:Int = 0 Until explicitDependencyLinks.length
			Local dependencyLink:TSyntaxLink = explicitDependencyLinks[d]
			If TTypeDeclarationSyntax(dependencyLink.GetSyntaxOrSyntaxToken()) Then
				' create edge link->dependencyLink
				edges[d] = TGraphNode(graphNodeMap[dependencyLink])
			Else If TTypeSyntax(dependencyLink.GetSyntaxOrSyntaxToken()) Then
				' TODO: check before doing these casts:
				Local dependencyNamePartSyntaxes:TQualifiedNamePartSyntax[] = TQualifiedNameTypeBaseSyntax(TTypeSyntax(dependencyLink.GetSyntaxOrSyntaxToken()).base).name.parts
				Function PartStr:String(s:TQualifiedNamePartSyntax) Return s.identifier.lexerToken.value End Function
				Local dependencyDeclarations:IDeclaration[] = GetParentCodeBlockScope(link, scopes).LookUpDeclarationWIP(PartStr(dependencyNamePartSyntaxes[0]).ToLower(), False)
				Assert dependencyDeclarations.length = 1 Else "wtf"
				' TODO: handle length = 0 (dependency doesnt exist)
				' look up dummy declaration and then create edge
				edges[d] = TGraphNode(graphNodeMap[TUnresolvedTypeDeclaration(dependencyDeclarations[0]).link])
			Else
				RuntimeError "Missing case"
			End If
		Next
		TGraphNode(graphNodeMap[link]).edges = edges
	Next
	Local sccs:TList = TopologicalSort(graphNodes) '<TList<TGraphNode>>
	Print "sccs:"
	For Local scc:TList = EachIn sccs
		Local str:String
		For Local n:TGraphNode = EachIn scc
			If str Then str :+ " <-> "
			str :+ TUnresolvedTypeDeclaration(n.obj).GetName()
		Next
		Print str
	Next
End Function
