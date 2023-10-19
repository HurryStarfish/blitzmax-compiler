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

Function GetScope:TScope(syntax:ISyntax, scopes:TMap)'<ISyntax, TScope>
	' gets the scope for this node, or, if none present, the enclosing scope
	Repeat
		Local scope:TScope = TScope(scopes[syntax])
		If scope Then Return scope
		syntax = syntax.Parent()
		If Not syntax Then Return Null
	Forever
End Function

Public
Type TCreateScopesVisitor Extends TSyntaxVisitor Final
	Field ReadOnly scopes:TMap = New TMap'<ISyntax, TScope>
	
	Method VisitTopDown(syntax:TCodeBlockSyntax)
		Local scope:TScope = TScope(scopes[syntax])
		Local parentSyntax:ISyntax = syntax.Parent()
		Local inheritsLocals:Int = IStatementSyntax(parentSyntax) Or ..
		                           TIfBranchSyntax(parentSyntax) Or ..
		                           TSelectBranchSyntax(parentSyntax) Or ..
		                           TTryBranchSyntax(parentSyntax) Or ..
		                           TIncludeDirectiveSyntax(parentSyntax.Parent()) And TIncludeDirectiveSyntax(parentSyntax.Parent()).Body().Block() = syntax
		scope = New TScope(GetScope(parentSyntax, scopes), inheritsLocals)
		scopes[syntax] = scope
		' TODO
	End Method
	
	Method VisitTopDown(syntax:TEnumDeclarationSyntax)
		scopes[syntax] = New TScope(GetScope(syntax.Parent(), scopes), False)
	End Method
End Type

Type TCollectTypeDeclarationSyntaxesVisitor Extends TSyntaxVisitor Final
	' see CreateTypeDeclarations
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
	Field ReadOnly typeDeclarationSyntaxes:TList = New TList'<TTypeDeclarationSyntax>
	
	Method VisitTopDown(syntax:TTypeDeclarationSyntax)
		typeDeclarationSyntaxes.AddLast syntax
	End Method
End Type

Type TCreateScopesAndInsertDeclarationsVisitor Extends TSyntaxVisitor Final
	Field ReadOnly scopes:TMap = New TMap'<ISyntax, TScope>
	
	Method VisitTopDown(syntax:TVariableDeclarationSyntax)
		' all declarations that aren't direct children of a code block
		' (callable parameters, For loop counters, Catch exception variables) should
		' be handled by their parent statements (see other visit methods below); since
		' these are top-down visits, that means they have already been handled when we get here
		If Not TCodeBlockSyntax(syntax.Parent()) Then Return
		
		Local scope:TScope = GetParentCodeBlockScope(syntax)
		' declarations without a declaration keyword are never direct children of a code block
		AddSymbolsForDeclaratorsToScope syntax, scope, True, EAllowedDeclaratorCount.AtLeastOne
	End Method
	
	Method VisitTopDown(syntax:TEnumMemberDeclarationSyntax)
		Local e:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax.Parent())
		Assert e Else "Parent of enum member is not an enum declaration"
		Local nameStr:String = syntax.Name().Identifier().lexerToken.value
		Local scope:TScope = TScope(scopes[e])
		scope.AddDeclaration New TConstDeclaration(nameStr)
	End Method
	
	Method VisitTopDown(syntax:TCallableDeclarationSyntax)
		Local scope:TScope = GetParentCodeBlockScope(syntax)
		Local nameStr:String
		If syntax.Name().IdentifierName() Then
			Local nameStr:String = syntax.Name().IdentifierName().Identifier().lexerToken.value
			Local declaration:TCallableDeclaration
			Select syntax.InitiatorKeyword().Kind()
				Case TTokenKind.Function_ declaration = New TFunctionDeclaration(nameStr)
				Case TTokenKind.Method_ declaration = New TMethodDeclaration(nameStr)
				Default RuntimeError "Missing case"
			End Select
			scope.AddDeclaration declaration
		Else If syntax.Name().KeywordName() Then
			Local declaration:TCallableDeclaration
			Select syntax.Name().KeywordName().Kind()
				Case TTokenKind.New_ declaration = New TConstructorDeclaration(nameStr)
				Case TTokenKind.Delete_ declaration = New TFinalizerDeclaration(nameStr)
				Default RuntimeError "Missing case"
			End Select
			scope.AddDeclaration declaration
		Else If syntax.Name().OperatorName() Then
			'TOperatorSyntax
			Throw "TODO"
		Else
			RuntimeError "Missing case"
		End If
		
		If syntax.Body() Then
			' parameter variable declarations
			Assert syntax.Type_().Suffixes().length >= 1 Else "Callable declaration is missing type suffixes"
			Assert TCallableTypeSuffixSyntax(syntax.Type_().Suffixes()[syntax.Type_().Suffixes().length - 1]) Else "Callable declaration is missing callable type suffix"
			Local callableTypeSuffixSyntax:TCallableTypeSuffixSyntax = TCallableTypeSuffixSyntax(syntax.Type_().Suffixes()[syntax.Type_().Suffixes().length - 1])
			If Not callableTypeSuffixSyntax Then ' parameter list was missing in code
				Throw "TODO" ' compile error
				Return
			End If
			Local bodyScope:TScope = CreateAndAttachScope(syntax.Body(), False)
			AddSymbolsForDeclaratorsToScope callableTypeSuffixSyntax.ParameterDeclaration(), bodyScope, False, EAllowedDeclaratorCount.Any
		End If
		
		' TODO: handle type parameters
	End Method
	
	Method VisitTopDown(syntax:TForStatementSyntax)
		' counter variable declaration
		If TForCounterDeclarationSyntax(syntax.Counter()) Then
			Local declaration:TVariableDeclarationSyntax = TForCounterDeclarationSyntax(syntax.Counter()).Declaration()
			Local bodyScope:TScope = CreateAndAttachScope(syntax.Body(), False)
			AddSymbolsForDeclaratorsToScope declaration, bodyScope, True, EAllowedDeclaratorCount.ExactlyOne
		End If
	End Method
	
	Method VisitTopDown(syntax:TCatchTryBranchSyntax)
		' exception variable declaration
		Local declaration:TVariableDeclarationSyntax = syntax.Declaration()
		Local bodyScope:TScope = CreateAndAttachScope(syntax.Body(), False)
		AddSymbolsForDeclaratorsToScope declaration, bodyScope, False, EAllowedDeclaratorCount.ExactlyOne
	End Method
	
	Private
	Method GetParentCodeBlockScope:TScope(syntax:ISyntax) ' requires the scope to exist already
		Assert TCodeBlockSyntax(syntax.Parent()) Else "Declaration is not in a block"
		Local scope:TScope = TScope(scopes[syntax.Parent()])
		Assert scope Else "Missing scope"
		Return scope
	End Method
	
	Method CreateAndAttachScope:TScope(syntax:TCodeBlockSyntax, inheritsLocalsFromParent:Int) ' requires the scope to not exist yet
		Assert Not scopes.Contains(syntax) Else "Code block unexpectedly already has a scope"
		Local scope:TScope = New TScope(GetScope(syntax.Parent(), scopes), inheritsLocalsFromParent)
		scopes[syntax] = scope
		Return scope
	End Method
	
	Function AddSymbolsForDeclaratorsToScope(declaration:TVariableDeclarationSyntax, scope:TScope, shouldHaveDeclarationKeywords:Int, allowedDeclaratorCount:EAllowedDeclaratorCount)
		Local declarationKeyword:TSyntaxToken = declaration.DeclarationKeyword()
		If Not declarationKeyword And shouldHaveDeclarationKeywords Then
			Throw "TODO"
			Return
		Else If declarationKeyword And Not shouldHaveDeclarationKeywords Then
			Throw "TODO"
			' only report error but continue
		End If
		
		' TODO: allow/disallow readonly
		
		Local elements:TVariableDeclaratorListElementSyntax[] = declaration.Declarators().Elements()
		Select allowedDeclaratorCount
			Case EAllowedDeclaratorCount.ExactlyOne
				If elements.length < 1 Then
					Throw "TODO"
				Else If elements.length > 1 Then
					Throw "TODO" ' this should haveve been handled by the parser already
					ProcessDeclarator elements[0], scope, declarationKeyword, shouldHaveDeclarationKeywords
				Else
					ProcessDeclarator elements[0], scope, declarationKeyword, shouldHaveDeclarationKeywords
				End If
			Case EAllowedDeclaratorCount.AtLeastOne
				If elements.length >= 1 Then
					For Local d:Int = 0 Until elements.length
						ProcessDeclarator elements[d], scope, declarationKeyword, shouldHaveDeclarationKeywords
					Next
				Else
					Throw "TODO"
				End If
			Case EAllowedDeclaratorCount.Any
				For Local d:Int = 0 Until elements.length
					ProcessDeclarator elements[d], scope, declarationKeyword, shouldHaveDeclarationKeywords
				Next
			Default RuntimeError "Missing case"
		End Select
		
		Function ProcessDeclarator(syntax:TVariableDeclaratorListElementSyntax, scope:TScope, declarationKeyword:TSyntaxToken, shouldHaveDeclarationKeyword:Int)
			Local nameStr:String = syntax.Declarator().Name().Identifier().lexerToken.value
			
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



Function CreateTypeDeclarations(typeDeclarationSyntaxes:TList, scopes:TMap)'<TTypeDeclarationSyntax>'<ISyntax, TScope>
	' creates declarations for all given syntax nodes and inserts them into scopes
	' since these declarations have references to all other declarations they have dependencies on,
	' this requires the would-be declarations to be topologically sorted before they can be created
	
	' such dependencies are:
	' - super types
	' - enum base types
	' - outer types (of types declared either directly inside other types, or locally inside callables)
	' - NOT type arguments of super/base types (because relationships such as such as
	'  "Type A Extends B<A>" must be allowed)
	' - NOT types referenced in generic constraints (because in that case, the dependency is that of
	'   the type parameter, not of the type having the type parameter)
	
	' TODO: add type parameters as their own declarations
	' - the generic type depends on its type parameters, so that the parameter declarations can be
	'   referenced by the generic declaration
	' TODO: add dependencies for generic super/base types (the dependency is on the type created by
	' applying the type arguments; this type will in turn have dependencies on the generic type it
	' was created from, as well as on all the type argument types - how to handle cycles there?)
	
	' e.g.: Type A Extends B<X>;   Type B<T> Where T : C
	'           - A depends on B<X>
	'           - B<X> depends on B<T> and on X
	'           - B<T> depends on T
	'           - T depends on C
	'           => creation order: C, T, B<T>, X, B<X>, A
	' e.g.: Type A Extends B<A>;   Type B<T>
	'           - A depends on B<A>
	'           - B<A> depends on B<T> and on A
	'           - B<T> depends on T
	'           => creation order: C, T, B<T>, X, B<X>, A
	
	' imports cannot have cycles, so types in imported compilation units must already have been resolved
	' but within the same unit, types can depend on each other regardless of scope nesting and code order
	' hence, this function should only be called once per compilation unit and given all type declaration
	' syntax links in the unit
		
	'Local resolutionQueue:TList = New TList'<TSyntaxLink<TTypeDeclarationSyntax>>
	
	' create list of unresolved placeholder declarations and insert into scopes if possible
	' these will be used for the dependency graph
	Local typeDeclarations:TList = New TList'<TTypeDeclaration>
	For Local syntax:TTypeDeclarationSyntax = EachIn typeDeclarationSyntaxes
		Local unresolvedDeclaration:TUnresolvedTypeDeclaration = New TUnresolvedTypeDeclaration(GetName(syntax), syntax)
		typeDeclarations.AddLast unresolvedDeclaration
		GetParentCodeBlockScope(syntax, scopes).AddDeclaration unresolvedDeclaration
		' TODO: handle failure to add (duplicate declaration)
	Next
	
	' create dependency graph
	Local graphNodes:TList = New TList'<TGraphNode>
	Local graphNodeMap:TMap = New TMap'<TTypeDeclarationSyntax, TGraphNode>
	Local typeDeclarationListLink:TLink = typeDeclarations.FirstLink()
	For Local syntax:ISyntax = EachIn typeDeclarationSyntaxes
		Local node:TGraphNode = New TGraphNode(typeDeclarationListLink.Value())
		graphNodes.AddLast node
		graphNodeMap[syntax] = node
		typeDeclarationListLink = typeDeclarationListLink.NextLink()
	Next
	For Local syntax:TTypeDeclarationSyntax = EachIn typeDeclarationSyntaxes
		Local explicitDependencySyntaxes:ISyntax[] = GetExplicitDependencies(syntax) '<TTypeSyntax | TTypeDeclarationSyntax>[]
		Local edges:TGraphNode[explicitDependencySyntaxes.length]
		For Local d:Int = 0 Until explicitDependencySyntaxes.length
			Local dependencySyntax:ISyntax = explicitDependencySyntaxes[d]
			If TTypeDeclarationSyntax(dependencySyntax) Then
				' create edge syntax->dependencySyntax
				edges[d] = TGraphNode(graphNodeMap[dependencySyntax])
			Else If TTypeSyntax(dependencySyntax) Then
				' TODO: check before doing these casts:
				Local dependencyNamePartSyntaxes:TQualifiedNamePartSyntax[] = TQualifiedNameTypeBaseSyntax(TTypeSyntax(dependencySyntax).Base()).Name().Parts()
				Function PartStr:String(s:TQualifiedNamePartSyntax) Return s.Identifier().lexerToken.value End Function
				Local dependencyDeclarations:IDeclaration[] = GetParentCodeBlockScope(syntax, scopes).LookUpDeclarationWIP(PartStr(dependencyNamePartSyntaxes[0]).ToLower(), False)
				If dependencyNamePartSyntaxes.length > 1 Then
					RuntimeError "TODO: lookup in more deeply nested scopes"
				End If
				Assert dependencyDeclarations.length = 1 Else "TODO: error handling for missing dependency"
				' TODO: handle length = 0 (dependency doesnt exist (declaration wasnt found))
				' look up placeholder declaration and then create edge
				edges[d] = TGraphNode(graphNodeMap[TUnresolvedTypeDeclaration(dependencyDeclarations[0]).syntax])
			Else
				RuntimeError "Missing case"
			End If
		Next
		TGraphNode(graphNodeMap[syntax]).edges = edges
	Next
	
	' create ordered list of strongly connected components from the dependency graph
	' if any component contains more than one node, it represents a dependency cycle
	' the list is ordered so that no node has a dependency on a node in a later component, which means
	' that (if there are no cycles) it describes a valid order to create the actual type declarations in
	Local stronglyConnectedComponents:TList = TopologicalSort(graphNodes) '<TList<TGraphNode>>
	
	DebugOutput stronglyConnectedComponents ' TODO: remove debug output (and standarddio import)
	Function DebugOutput(sccs:TList)
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
	
	' TODO: create the actual declarations and replace the placeholders
	
	Function GetExplicitDependencies:ISyntax[](syntax:TTypeDeclarationSyntax)'<TTypeSyntax | TTypeDeclarationSyntax>[]
		' does not include Object as an implicit super type or implicit base types of enums
		' those are declared in BRL.Blitz and guaranteed to already be available
		Local dependencies:ISyntax[]'<TTypeSyntax | TTypeDeclarationSyntax>[]
		
		' outer type
		If TTypeDeclarationSyntax(syntax.Parent()) Then
			dependencies :+ [syntax.Parent()]
		Else If TCodeBlockSyntax(syntax.Parent()) Then
			Local parentSyntax:ISyntax = syntax.Parent()
			While TCodeBlockSyntax(parentSyntax)
				If TTypeDeclarationSyntax(parentSyntax.Parent()) Then
					dependencies :+ [parentSyntax.Parent()]
					Exit
				End If
				parentSyntax = parentSyntax.Parent()
			Wend
		End If
		
		' TODO: error on direct self-reference (such as "T Extends T"),
		'       since this is not caught by the cycle detection

		' super/base types
		If TClassDeclarationSyntax(syntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(syntax)
			If syntax.TypeParameters() Then
				RuntimeError "TODO"
			End If
			If syntax.SuperClass() Then
				Local superClassSyntax:ISyntax = syntax.SuperClass()
				dependencies :+ [superClassSyntax]
				' TODO: handle generic super class
			End If
			If syntax.SuperInterfaces() Then
				For Local e:TTypeListElementSyntax = EachIn syntax.SuperInterfaces().Elements()
					If e.Type_() Then
						Local superInterfaceSyntax:TTypeSyntax = e.Type_()
						dependencies :+ [superInterfaceSyntax]
						' TODO: handle generic super interfaces
					End If
				Next
			End If
		Else If TStructDeclarationSyntax(syntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(syntax)
			If syntax.TypeParameters() Then
				RuntimeError "TODO"
			End If
			' TODO: handle generic super interfaces
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(syntax)
			If syntax.TypeParameters() Then
				RuntimeError "TODO"
			End If
			If syntax.SuperInterfaces() Then
				For Local e:TTypeListElementSyntax = EachIn syntax.SuperInterfaces().elements
					If e.Type_() Then
						Local superInterfaceSyntax:TTypeSyntax = e.Type_()
						dependencies :+ [superInterfaceSyntax]
						' TODO: handle generic super interfaces
					End If
				Next
			End If
		Else If TEnumDeclarationSyntax(syntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax)
			If syntax.BaseType() Then dependencies :+ [Syntax.BaseType()]
			' TODO: handle generic base type
		Else
			RuntimeError "Missing case"
		End If
		
		Return dependencies
		
		Function GetTypeArgumentDependencies:TTypeSyntax[](typeArgumentListSyntax:TTypeArgumentListSyntax)
			Local typeListSyntax:TTypeListSyntax = typeArgumentListSyntax.TypeList()
			Local dependencies:TTypeSyntax[]
			For Local e:TTypeListElementSyntax = EachIn typeListSyntax.Elements()
				Local typeArgumentSyntax:TTypeSyntax = e.Type_()
				
				' TODO: the type argument can itself have type arguments
				'       but in that case, the actual dependency is not on the type belonging to the
				'       type argument's declaration, but on the type produced by applying its
				'       type arguments to it
				'       e.g.: Type A Extends B<C>; Type B<T>
				'                - A depends on B<C>
				'                - B<C> depends on B<T> and on C
				'                - B does not depend on A or C
				If typeArgumentSyntax.TypeArguments() Then
					RuntimeError "TODO"
					'Local typeArgumentTypeArgumentListSyntax:ISyntax = typeArgumentSyntax.TypeArguments()
					'dependencies :+ GetTypeArgumentDependencies(typeArgumentTypeArgumentListSyntax)
				End If

				dependencies :+ [typeArgumentSyntax]
			Next
			Return dependencies
		End Function
	End Function
	
	Function GetParentCodeBlockScope:TScope(syntax:ISyntax, scopes:TMap)
		Assert TCodeBlockSyntax(syntax.Parent()) Else "Declaration is not in a block"
		Local scope:TScope = TScope(scopes[syntax.Parent()])
		Assert scope Else "Missing scope"
		Return scope
	End Function
	
	Function GetName:String(syntax:TTypeDeclarationSyntax)
		If TClassDeclarationSyntax(syntax) Then
			Return TClassDeclarationSyntax(syntax).Name().Identifier().lexerToken.value
		Else If TStructDeclarationSyntax(syntax) Then
			Return TStructDeclarationSyntax(syntax).Name().Identifier().lexerToken.value
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Return TInterfaceDeclarationSyntax(syntax).Name().Identifier().lexerToken.value
		Else If TEnumDeclarationSyntax(syntax) Then
			Return TEnumDeclarationSyntax(syntax).Name().Identifier().lexerToken.value
		Else
			RuntimeError "Missing case"
		End If
	End Function
	
	Type TUnresolvedTypeDeclaration Extends TTypeDeclaration Final
		Field ReadOnly syntax:TTypeDeclarationSyntax
		
		Method New(name:String, syntax:TTypeDeclarationSyntax)
			Self.name = name
			Self.syntax = syntax
		End Method
		
		Method GetSyntax:TTypeDeclarationSyntax() Override
			RuntimeError "no"
		End Method
	End Type
End Function
