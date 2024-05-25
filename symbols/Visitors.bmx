SuperStrict
Import "Scope.bmx"
Import "../syntax/SyntaxVisitor.bmx"
Import "../util/GraphUtils.bmx"
Import "SymbolsImpl.bmx"
Import "SemanticError.bmx"
Import "../util/MessageUtils.bmx"
Import "../util/Logging.bmx"
Import BRL.LinkedList
Import BRL.Map
Import brl.standardio ' TODO: remove again


' TODO: https://learn.microsoft.com/en-us/archive/blogs/ericlippert/how-do-we-ensure-that-method-type-inference-terminates


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
		                           TIncludedCodeSyntax(parentSyntax.Parent()) And TIncludedCodeSyntax(parentSyntax.Parent()).Body().Block() = syntax
		scope = New TScope(GetScope(parentSyntax, scopes), inheritsLocals)
		scopes[syntax] = scope
		' TODO
	End Method
	
	Method VisitTopDown(syntax:TEnumDeclarationSyntax)
		scopes[syntax] = New TScope(GetScope(syntax.Parent(), scopes), False)
	End Method
End Type

Type TCollectTypeDeclarationSyntaxesVisitor Extends TSyntaxVisitor Final
	Field ReadOnly typeDeclarationSyntaxes:TList = New TList'<TTypeDeclarationSyntax>
	
	Method VisitTopDown(syntax:TTypeDeclarationSyntax)
		typeDeclarationSyntaxes.AddLast syntax
	End Method
End Type

Type TCreateScopesAndInsertDeclarationsVisitor Extends TSyntaxVisitor Final
	Field ReadOnly scopes:TMap = New TMap'<ISyntax, TScope>
	
	Method VisitTopDown(syntax:TVariableDeclarationSyntax)
		' all declarations that aren't direct children of a code block
		' (e.g. callable parameters, For loop counters, Catch exception variables) should be
		' handled by their respective parent statements (see visit methods below); since these
		' are top-down visits, such declarations have already been handled when see them here
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
		RuntimeError "TODO"'scope.AddDeclaration New TConstDeclarationSymbol(nameStr)
	End Method
	
	Method VisitTopDown(syntax:TCallableDeclarationSyntax)
		RuntimeError "TODO"
		Rem
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
		End Rem
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
			RuntimeError "TODO"
			Rem
			Local nameStr:String = syntax.Declarator().Name().Identifier().lexerToken.value
			
			Local symbol:IVariableDeclarationSymbol
			If (Not declarationKeyword) Or (Not shouldHaveDeclarationKeyword) Then
				'                                   ^ ignore keyword if unexpected
				symbol = New TLocalDeclaration(nameStr)
			Else
				Select declarationKeyword.Kind()
					Case TTokenKind.Const_  symbol = New TConstDeclarationSymbol(nameStr)
					Case TTokenKind.Global_ symbol = New TGlobalDeclarationSymbol(nameStr)
					Case TTokenKind.Local_  symbol = New TLocalDeclarationSymbol(nameStr)
					Case TTokenKind.Field_  symbol = New TFieldDeclarationSymbol(nameStr)
					Default RuntimeError "Missing case"
				End Select
			End If
			'If scope.GetSymbolForName(nameStr) Then Throw "TODO" ' TODO: handle duplicate declarations
			scope.AddDeclaration symbol
			End Rem
		End Function
	End Function
End Type



Function CreateTypeDeclarations(typeDeclarationSyntaxes:TTypeDeclarationSyntax[], scopeTree:TScopeTree)'<ISyntax, TScope>
	' creates type declarations for all given syntax nodes and inserts them into scopes
	' this function should only be called once be compilation unit, with all of the unit's
	' type declaration syntaxes
	' precondition: scopes have already been attached to the type body syntaxes
	
	' these dependencies can span across compilation units, can span across scopes even within the same
	' compilation unit (regardless of nesting structure), and are independent of code order
	' type declarations can have various kinds of relationships with each other
	' these include:
	' - super types
	' - enum base types
	' - outer types
	' - type arguments of super/base types
	' - types referenced in generic constraints
	' some of these relationships are allowed to form cycles while others do not
	' the following restrictions apply:
	' - a type may not inherit itself or a subtype of itself (no inheritance cycles)
	' - a type may not inherit one of its type parameters
	' however:
	' - a type may inherit one of its outer or inner types
	' - a type may use itself as a type argument of one of its super types (e.g. "Type A Extends B<A>" is allowed)
	' - a type may use itself as a constraint (or type argument thereof) for one of its type parameters (e.g. "Type A<T> Where T Extends A<T>")
	
	' since imports cannot form cycles, inheritance cycles can only occur between types in the same
	' compilation unit; types in imported compilation units must already be resolved
	' within a compilation unit however, types can depend on each other regardless of code
	' order and nesting
	
	' type declarations are created and initialized using the following algorithm:
	' - create unfinished type declarations for all types in the compilation unit and insert them
	'   into the correct scopes
	' - for each declaration, create a node for a directed graph
	' - repeat for all unfinished types in the order of the corresponding nodes:
	'   - if type has no super types that might be from the same compilation unit, mark as finished
	'   - otherwise, try to look up super type declarations
	'     - if lookup involves looking into the body scope of a type that is unfinished
	'       or has any super types that are themselves unfinished*, cancel the lookup
	'       (because otherwise the result of the lookup might be incomplete) and record an edge
	'       towards said type whose scope caused the lookup to be cancelled
	'       * ignore any super type that is the type currently being processed
	'     - otherwise, if all lookups complete without being cancelled, mark the type as finished,
	'       discard previous edges from this type and record edges towards all immediate super types
	'   - topologically sort the nodes for the next iteration of the loop
	'       if all types are finished OR no progress has been made in the latest iteration,
	'       calculate strongly connected components to find cycles and report errors for them;
	'       also report errors for super types that were not looked up successfully
	
	
	' TODO: add type parameters as their own declarations
	'       (a generic type must allow its type parameter declarations to be referenced inside
	'       the rest of its signature; this means they must be in scope there)
	' TODO: add dependencies for generic super/base types (the dependency is on the type created by
	' applying the type arguments; this type will in turn have dependencies on the generic type it
	' was created from, as well as on all the type argument types - how to handle cycles there?)
	' e.g.: Type A Extends B<X>;   Type B<T> Where T : C
	'           - A depends on B<X>
	'           - B<X> depends on B<T> and on X
	'           - B<T> depends on T
	'           - T depends on C
	'           => resolution order: C, T, B<T>, X, B<X>, A?
	' e.g.: Type A Extends B<A>;   Type B<T>
	'           - A depends on B<A>
	'           - B<A> depends on B<T> and on A
	'           - B<T> depends on T
	'           => resolution order: C, T, B<T>, X, B<X>, A?
	
	' TODO: should a type be allowed to derive from a type less visible than it?
	'       if so, the derived type should probably not be allowed to have any callable member that
	'       (1) has its super type (this type) mentioned as or in one of its parameter types and
	'       (2) can still be overridden (i.e. the derived type and callable are both non-final and the
	'       callable is not already overridden by another one that does not match conditions (1) and (2))
	'       otherwise, when deriving from the derived type in a place where the base type is not visible,
	'       it would be impossible to override such callables because for that, the parameter types must
	'       match exactly (otherwise it's an overload) and the base type is not visible so they can't
	
	Type TBuilderData Final
		Field ReadOnly builder:TTypeDeclarationSymbolBuilder
		Field ReadOnly graphNode:TGraphNode
		Field ReadOnly superTypeDatas:TSuperTypeData[]
		Field finished:Int = False
		Field allSuperTypesFinished:Int = False
		Method New(builder:TTypeDeclarationSymbolBuilder, graphNode:TGraphNode, superTypeDatas:TSuperTypeData[])
			Self.builder = builder
			Self.graphNode = graphNode
			Self.superTypeDatas = superTypeDatas
		End Method
		Method SuperTypesFinished:Int(except:TBuilderData, builderDatas:TBuilderData[])
			' this does not need to be recursive, since name lookups will visit super types anyway
			If allSuperTypesFinished Then Return True
			Local skippedExcept:Int = False
			For Local superType:ITypeSymbol = EachIn Self.builder.Declaration().SuperTypes()
				Local superData:TBuilderData = TBuilderData.Find(superType.Declaration(), builderDatas)
				If Not superData Then Continue
				If except And except.builder = superData.builder Then skippedExcept = True; Continue
				If Not (superData.finished) Then Return False
			Next
			If Not except Or (skippedExcept And except.finished) Then allSuperTypesFinished = True
			Return True
		End Method
		Function Find:TBuilderData(declaration:ITypeDeclarationSymbol, builderDatas:TBuilderData[])
			For Local d:Int = 0 Until builderDatas.length
				If builderDatas[d].builder.Declaration() = declaration Then Return builderDatas[d]
			Next
			If Not IErrorTypeDeclarationSymbol(declaration) Then RuntimeError "TODO: Data not found (imported?)"
		End Function
	End Type
	
	Type TSuperTypeData Final
		Field typeAssigned:Int = False
		Field ReadOnly typeSyntax:TTypeSyntax ' for the type symbol
		Field ReadOnly typeNameSyntax:TQualifiedNameSyntax'nullable (null if different type base) ' for the lookup
		Field ReadOnly isSuperClass:Int
		Field ReadOnly isSuperInterface:Int
		Field ReadOnly superInterfaceIndex:Int
		Method New(typeSyntax:TTypeSyntax, isSuperClass:Int, isSuperInterface:Int, superInterfaceIndex:Int = -1)
			Assert isSuperClass Or isSuperInterface Else "Unspecified kind of super type"
			Assert (isSuperInterface And superInterfaceIndex <> -1) Or (Not isSuperInterface And superInterfaceIndex = -1) Else "Super interface flag does not match index"
			Self.typeSyntax = typeSyntax
			If typeSyntax And TQualifiedNameTypeBaseSyntax(typeSyntax.Base()) Then
				Self.typeNameSyntax = TQualifiedNameTypeBaseSyntax(typeSyntax.Base()).Name()
			End If
			Self.typeNameSyntax = typeNameSyntax
			Self.isSuperClass = isSuperClass
			Self.isSuperInterface = isSuperInterface
			Self.superInterfaceIndex = superInterfaceIndex
		End Method
	End Type
	Function CreateBuilder:TTypeDeclarationSymbolBuilder(syntax:TTypeDeclarationSyntax)
		If TClassDeclarationSyntax(syntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(syntax)
			Local builder:TClassDeclarationSymbolBuilder = New TClassDeclarationSymbolBuilder(syntax)
			If syntax.SuperInterfaces() Then
				Local nulls:TInterfaceSymbol[syntax.SuperInterfaces().Elements().length]
				builder.SetSuperInterfaces(nulls)
			End If
			If syntax.TypeParameters() Then RuntimeError "TODO"
			Return builder
		Else If TStructDeclarationSyntax(syntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(syntax)
			Local builder:TStructDeclarationSymbolBuilder = New TStructDeclarationSymbolBuilder(syntax)
			If syntax.SuperInterfaces() Then
				Local nulls:TInterfaceSymbol[syntax.SuperInterfaces().Elements().length]
				builder.SetSuperInterfaces(nulls)
			End If
			If syntax.TypeParameters() Then RuntimeError "TODO"
			Return builder
		Else If TInterfaceDeclarationSyntax(syntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(syntax)
			Local builder:TInterfaceDeclarationSymbolBuilder = New TInterfaceDeclarationSymbolBuilder(syntax)
			If syntax.SuperInterfaces() Then
				Local nulls:TInterfaceSymbol[syntax.SuperInterfaces().Elements().length]
				builder.SetSuperInterfaces(nulls)
			End If
			If syntax.TypeParameters() Then RuntimeError "TODO"
			Return builder
		Else If TEnumDeclarationSyntax(syntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(syntax)
			Local builder:TEnumDeclarationSymbolBuilder = New TEnumDeclarationSymbolBuilder(syntax)
			Return builder
		Else
			RuntimeError "Missing case"
		End If
	End Function
	Function CreateSuperTypeDatas:TSuperTypeData[](typeDeclarationSyntax:ISyntax)
		Local superTypeDatas:TSuperTypeData[]
		If TClassDeclarationSyntax(typeDeclarationSyntax) Then
			Local syntax:TClassDeclarationSyntax = TClassDeclarationSyntax(typeDeclarationSyntax)
			If syntax.SuperClass() Then
				superTypeDatas = [New TSuperTypeData(syntax.SuperClass(), True, False)]
			End If
			If syntax.SuperInterfaces() Then
				Local datas:TSuperTypeData[syntax.SuperInterfaces().Elements().length]
					For Local d:Int = 0 Until datas.length
					datas[d] = New TSuperTypeData(syntax.SuperInterfaces().Elements()[d].Type_(), False, True, d)
				Next
				superTypeDatas :+ datas
			End If
		Else If TStructDeclarationSyntax(typeDeclarationSyntax) Then
			Local syntax:TStructDeclarationSyntax = TStructDeclarationSyntax(typeDeclarationSyntax)
			If syntax.SuperInterfaces() Then
				Local datas:TSuperTypeData[syntax.SuperInterfaces().Elements().length]
					For Local d:Int = 0 Until datas.length
					datas[d] = New TSuperTypeData(syntax.SuperInterfaces().Elements()[d].Type_(), False, True, d)
				Next
				superTypeDatas :+ datas
			End If
		Else If TInterfaceDeclarationSyntax(typeDeclarationSyntax) Then
			Local syntax:TInterfaceDeclarationSyntax = TInterfaceDeclarationSyntax(typeDeclarationSyntax)
			If syntax.SuperInterfaces() Then
				Local datas:TSuperTypeData[syntax.SuperInterfaces().Elements().length]
					For Local d:Int = 0 Until datas.length
					datas[d] = New TSuperTypeData(syntax.SuperInterfaces().Elements()[d].Type_(), False, True, d)
				Next
				superTypeDatas :+ datas
			End If
		Else If TEnumDeclarationSyntax(typeDeclarationSyntax) Then
			Local syntax:TEnumDeclarationSyntax = TEnumDeclarationSyntax(typeDeclarationSyntax)
			Return []
		Else If TTypeParameterDeclaratorSyntax(typeDeclarationSyntax) Then
			RuntimeError "TODO"
		Else
			RuntimeError "Missing case"
		End If
		Return superTypeDatas
	End Function
	
	
	' create unfinished declarations and insert into scopes if possible
	If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, "creating type declarations"	
	Local builderDatas:TBuilderData[typeDeclarationSyntaxes.length]
	Local graphNodes:TGraphNode[typeDeclarationSyntaxes.length]
	Local unfinishedTypeCount:Int = builderDatas.length
	Local unfinishedSuperCount:Int
	For Local s:Int = 0 Until builderDatas.length
		Local graphNode:TGraphNode = New TGraphNode(Null, [])
		Local syntax:TTypeDeclarationSyntax = typeDeclarationSyntaxes[s]
		Local builder:TTypeDeclarationSymbolBuilder = CreateBuilder(syntax) ' TODO: move to the visitor that collects the syntaxes?
		GetParentCodeBlockScope(syntax, scopeTree).AddDeclaration builder.Declaration()
		' TODO: handle failure to add (duplicate declaration)
		builderDatas[s] = New TBuilderData(builder, graphNode, CreateSuperTypeDatas(syntax))
		graphNode.obj = builderDatas[s]
		graphNodes[s] = graphNode
		unfinishedSuperCount :+ builderDatas[s].superTypeDatas.length
	Next
	
	Local unfinishedGraphNodes:TGraphNode[] = graphNodes
	Repeat
		Local unfinishedSuperCountAtStartOfPass:Int = unfinishedSuperCount
		' iterate over nodes for unfinished type declarations
		#GraphNodeLoop
		For Local graphNode:TGraphNode = EachIn unfinishedGraphNodes
			Local data:TBuilderData = TBuilderData(graphNode.obj)
			
			If Not data.superTypeDatas Then ' TODO: or if they are imported types
				' no explicit super types
				If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, data.builder.Declaration().Name() + ": finished because no supers"
				' TODO: set to Object
				data.finished = True
				data.allSuperTypesFinished = True
				unfinishedTypeCount :- 1
			Else
				Local anySuperTypesMissing:Int = False
				' iterate over super type datas for the type
				#SuperTypeDataLoop
				For Local superTypeData:TSuperTypeData = EachIn data.superTypeDatas
					If superTypeData.typeAssigned Then Continue
					
					If Not superTypeData.typeNameSyntax Then
						RuntimeError "TODO: keyword/sigil types, or missing type due to syntax error"
					Else If superTypeData.typeSyntax.Suffixes() Then
						RuntimeError "TODO: array types etc"
					End If
					
					' start lookups in the scope the declaration is in
					Local lookupStartingScope:TScope = GetParentCodeBlockScope(data.builder.Declaration().Syntax(), scopeTree)
					' iterate over parts of the super type name
					Local nameSyntax:TQualifiedNameSyntax = superTypeData.typeNameSyntax
					Local parts:TQualifiedNamePartSyntax[] = nameSyntax.Parts()
					If Not parts[0].Identifier() Then RuntimeError "TODO: global scope lookup"
					For Local p:Int = 0 Until parts.length
						Local part:TQualifiedNamePartSyntax = parts[p]
						Local isFirstPart:Int = p = 0
						Local isLastPart:Int = p = parts.length - 1
						
						' look up declaration(s) for the current name part
						' cancel the lookup if the scope corresponds to the body of a type that is
						' unfinished  (with the exception of the type currently being processed)
						' or has any unfinished super types; otherwise, the result would be incomplete
						Type LookupCancelCondition Implements ILookupCancelCondition Final
							Field ReadOnly data:TBuilderData
							Field ReadOnly scopeTree:TScopeTree
							Field ReadOnly builderDatas:TBuilderData[]
							
							Field cancelledAtScope:TScope = Null
							
							Method New(data:TBuilderData, scopeTree:TScopeTree, builderDatas:TBuilderData[])
								Self.data = data
								Self.scopeTree = scopeTree
								Self.builderDatas = builderDatas
							End Method
							
							Method LookupCancelCondition:Int(scope:TScope) Override
								For Local d:TBuilderData = EachIn builderDatas
									' find the type/data that corresponds to the scope being looked in
									If d = data Then Continue ' always allow self-lookup (e.g. "T Extends T.U")
									If Not (d.finished And d.SuperTypesFinished(data, builderDatas)) Then
										Local s:TScope = TScope(scopeTree.scopes[d.builder.Declaration().BodySyntax()])
										Assert s Else "Missing scope"
										If s = scope Then cancelledAtScope = scope; Return True
									End If
								Next
							End Method
						End Type
						Local cancelCondition:LookupCancelCondition = New LookupCancelCondition(data, scopeTree, builderDatas)
						
						If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, data.builder.Declaration().Name() + ": looking up " + part.Identifier().lexerToken.value
						Local foundDeclarations:IDeclarationSymbol[] = scopeTree.LookUpTypeDeclaration(lookupStartingScope, part.Identifier().lexerToken.value, isFirstPart, cancelCondition)
						
						If cancelCondition.cancelledAtScope Then
							' record edge to the unfinished type that caused the lookup to cancel
							For Local c:Int = 0 Until builderDatas.length
								If scopeTree.scopes[builderDatas[c].builder.Declaration().BodySyntax()] = cancelCondition.cancelledAtScope Then
									Local cancelledAtData:TBuilderData = builderDatas[c]
									Local alreadyHasThisEdge:Int = False
									For Local edge:TGraphNode = EachIn data.graphNode.edges
										If edge = cancelledAtData.graphNode Then alreadyHasThisEdge = True; Exit
									Next
									If Not alreadyHasThisEdge Then data.graphNode.edges :+ [cancelledAtData.graphNode]
									If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, data.builder.Declaration().Name() + ": skipped because of attempted lookup in " + cancelledAtData.builder.Declaration().Name()
									Exit
								End If
								Assert data.builder <> builderDatas[builderDatas.length - 1].builder Else "TODO: Unable to create edge; finished or imported type?"
							Next
							anySuperTypesMissing = True
							Continue SuperTypeDataLoop
						End If
						
						' lookup completed;
						' select best match for this name part from the found declarations
						' if the name part is not the last part, a nullary result is expected;
						' otherwise the expected arity depends on the supplied type argument list, if any
						' if multiple matches were found, then an ambiguity error is reported
						' unless a single best match can be determined
						' e.g.: if looking up part B in A.B.C finds a "Type B" and "Interface B<T>",
						' then the former is the best match, but if it finds a "Type B" and "Interface B",
						' then there is ambiguity
						' TODO: how to handle ambiguity involving type parameter default types,
						' e.g. "Type B" and "Interface B<T = Int>"?
						Local foundTypeDeclarations:ITypeDeclarationSymbol[foundDeclarations.length]
						For Local d:Int = 0 Until foundTypeDeclarations.length
							If ITypeDeclarationSymbol(foundDeclarations[d]) Then
								foundTypeDeclarations[d] = ITypeDeclarationSymbol(foundDeclarations[d])
							Else
								RuntimeError "TODO: found something that is not a type"
							End If
						Next
						If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, data.builder.Declaration().Name() + ": found " + foundTypeDeclarations.length + " declarations for " + part.Identifier().lexerToken.value
						If Not foundTypeDeclarations Then
							' found no declarations
							If isLastPart Then
								ReportError "Cannot find type " + part.Identifier().lexerToken.value, part
							Else
								ReportError "Cannot find type or module " + part.Identifier().lexerToken.value, part
							End If
							SetSuperType data.builder, superTypeData, ErrorTypeDeclarationSymbol
							superTypeData.typeAssigned = True
							unfinishedSuperCount :- 1
						Else
							Local desiredArity:Int
							If isLastPart And superTypeData.typeSyntax.TypeArguments() Then
								desiredArity = superTypeData.typeSyntax.TypeArguments().TypeList().Elements().length
							Else
								desiredArity = 0
							End If
							Local bestMatches:ITypeDeclarationSymbol[]
							For Local declaration:ITypeDeclarationSymbol = EachIn foundTypeDeclarations
								If declaration.Arity() = desiredArity Then bestMatches :+ [declaration]
							Next
							
							If bestMatches.length = 1 Then
								' found exactly one declaration that matches
								' TODO: find modules too (for non-last parts), not just types
								Local bestMatch:ITypeDeclarationSymbol = bestMatches[0]
								If isLastPart Then
									If bestMatch = data.builder.Declaration() Then ' direct self-inheritance
										ReportError "Self-inheritance of " + data.builder.Declaration().NameWithTypeParameters(), superTypeData.typeSyntax
										SetSuperType data.builder, superTypeData, ErrorTypeDeclarationSymbol
									Else
										SetSuperType data.builder, superTypeData, bestMatch
									End If
									superTypeData.typeAssigned = True
									unfinishedSuperCount :- 1
								Else
									Assert TScope(scopeTree.scopes[bestMatch.BodySyntax()]) Else "Missing scope"
									lookupStartingScope = TScope(scopeTree.scopes[bestMatch.BodySyntax()])
								End If
							Else If bestMatches.length = 0 And foundTypeDeclarations.length = 1 Then
								' found exactly one declaration, but it does not match
								ReportError foundTypeDeclarations[0].NameWithTypeParameters() + " requires " + foundTypeDeclarations[0].Arity() + Pluralize(" type argument", foundTypeDeclarations[0].Arity() <> 1) + ", not " + desiredArity, part
								SetSuperType data.builder, superTypeData, ErrorTypeDeclarationSymbol
								superTypeData.typeAssigned = True
								unfinishedSuperCount :- 1
							Else If bestMatches.length > 1 Then
								' found multiple declarations that match
								Local bestMatchNamesStr:String = ""
								For Local m:Int = 0 Until bestMatches.length
									If bestMatchNamesStr Then bestMatchNamesStr :+ [", ", " and "][m = bestMatches.length - 1]
									' TODO: report fully qualified names (or at least relative to the current location) here
									bestMatchNamesStr :+ bestMatches[m].NameWithTypeParameters()
								Next
								ReportError "~q" + part.Identifier().lexerToken.value + "~q is ambiguous between " + bestMatchNamesStr, part
								SetSuperType data.builder, superTypeData, ErrorTypeDeclarationSymbol
								superTypeData.typeAssigned = True
								unfinishedSuperCount :- 1
							Else
								' found multiple declarations, but none of them match
								Local foundTypeDeclarationNamesStr:String = ""
								For Local d:Int = 0 Until foundTypeDeclarations.length
									If foundTypeDeclarationNamesStr Then foundTypeDeclarationNamesStr :+ ", "'[", ", ", "][d = foundTypeDeclarations.length - 1]]
									' TODO: report fully qualified names (or at least relative to the current location) here
									foundTypeDeclarationNamesStr :+ foundTypeDeclarations[d].NameWithTypeParameters()
								Next
								ReportError "Cannot find a type " + part.Identifier().lexerToken.value + " with " + desiredArity + Pluralize(" type parameter", desiredArity <> 1) + "; available types are: " + foundTypeDeclarationNamesStr, part
								SetSuperType data.builder, superTypeData, ErrorTypeDeclarationSymbol
								superTypeData.typeAssigned = True
								unfinishedSuperCount :- 1
							End If
						End If
					Next
				Next
				
				If Not anySuperTypesMissing Then
					' all lookups for all super types completed, so mark the type as finished
					' and record edges to all super types
					If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, data.builder.Declaration().Name() + ": finished because all lookups done"
					data.finished = True
					unfinishedTypeCount :- 1
					data.graphNode.edges = []
					For Local superType:ITypeSymbol = EachIn data.builder.Declaration().SuperTypes()
						Local superData:TBuilderData = TBuilderData.Find(superType.Declaration(), builderDatas)
						If superData Then data.graphNode.edges :+ [superData.graphNode]
					Next
				End If
			End If
		Next
		

		If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, unfinishedTypeCount + " unfinished types, " + unfinishedSuperCount + " unfinished supers remaining"
		
		If unfinishedTypeCount > 0 And unfinishedSuperCount < unfinishedSuperCountAtStartOfPass Then
			' making progress but not done yet
			' do SCC/topological sort of the remaining unfinished nodes to determine the
			' best order for the next pass
			Local stronglyConnectedComponents:TGraphNode[][] = TopologicalSort(unfinishedGraphNodes)
			If LoggingActive(ELogCategory.TypeCreation) Then LogNodesAndComponents unfinishedGraphNodes, stronglyConnectedComponents, builderDatas ' TODO: remove debug output (and standardio import)
			unfinishedGraphNodes = FlattenAndRemoveFinished(stronglyConnectedComponents)
			Function FlattenAndRemoveFinished:TGraphNode[](graphNodes:TGraphNode[][])
				Local result:TGraphNode[]
				For Local nn:TGraphNode[] = EachIn graphNodes
					For Local n:TGraphNode = EachIn nn
						If Not TBuilderData(n.obj).finished Then result :+ [n]
					Next
				Next
				Return result
			End Function
		Else
			' all finished or no more progress
			' do SCC/topological sort over all nodes to find cycles and report errors
			Local stronglyConnectedComponents:TGraphNode[][] = TopologicalSort(graphNodes)
			If LoggingActive(ELogCategory.TypeCreation) Then LogNodesAndComponents unfinishedGraphNodes, stronglyConnectedComponents, builderDatas ' TODO: remove debug output (and standardio import)
			
			' set missing dependencies to error type
			For Local graphNode:TGraphNode = EachIn unfinishedGraphNodes
				Local data:TBuilderData = TBuilderData(graphNode.obj)
				For Local superTypeData:TSuperTypeData = EachIn data.superTypeDatas
					If Not superTypeData.typeAssigned Then SetSuperType data.builder, superTypeData, ErrorTypeDeclarationSymbol
				Next
			Next
			
			' report errors for inheritance or lookup dependency cycles
			' (except for direct self-inheritance, which was already reported before)
			For Local component:TGraphNode[] = EachIn stronglyConnectedComponents
				If component.length > 1 Then ' cyclic dependency
					' in the case of cyclic dependencies, every affected super type for every type
					' in this component will either show up as another type in the component
					' (e.g. "Type A Extends B" + "Type B Extends A") or will already be the error type
					' (e.g. "Type A Extends B.BB" + "Type B Extends A.AA")
					' in the former case, all these super types will be replaced with the error type
					Local componentDeclarationNamesStr:String = ""
					For Local n:Int = 0 Until component.length
						If componentDeclarationNamesStr Then componentDeclarationNamesStr :+ [", ", " and "][n = component.length - 1]
						componentDeclarationNamesStr :+ TBuilderData(component[n].obj).builder.Declaration().NameWithTypeParameters()
					Next
					For Local graphNode:TGraphNode = EachIn component
						Local data:TBuilderData = TBuilderData(graphNode.obj)
						Local superTypes:ITypeSymbol[] = data.builder.Declaration().SuperTypes()
						Assert superTypes.length = data.superTypeDatas.length Else "Super type count does not match data count"
						For Local s:Int = 0 Until data.superTypeDatas.length
							If IErrorTypeSymbol(superTypes[s]) Then
								ReportError "Cyclic dependency between " + componentDeclarationNamesStr, superTypes[s].Syntax()
							Else
								For Local otherGraphNode:TGraphNode = EachIn component
									Local otherData:TBuilderData = TBuilderData(otherGraphNode.obj)
									If superTypes[s].Declaration() = otherData.builder.Declaration() Then
										'ReportError "Cyclic inheritance between " + componentDeclarationNamesStr, data.superTypeDatas[s].typeSyntax
										ReportError "Cyclic dependency between " + componentDeclarationNamesStr, data.superTypeDatas[s].typeSyntax
										SetSuperType data.builder, data.superTypeDatas[s], ErrorTypeDeclarationSymbol
									End If
								Next
							End If
						Next
					Next
				End If
			Next
			
			Exit
		End If
	Forever
	
	If LoggingActive(ELogCategory.TypeCreation) Then
		Log ELogCategory.TypeCreation, "type creation done"
		Log ELogCategory.TypeCreation, "processed types:"
		For Local graphNode:TGraphNode = EachIn GraphNodes
			Local data:TBuilderData = TBuilderData(graphNode.obj)
			Log ELogCategory.TypeCreation, data.builder.Declaration().Name()
			For Local s:ITypeSymbol = EachIn data.builder.Declaration().SuperTypes()
				Log ELogCategory.TypeCreation, "  " + s.Declaration().Name()
			Next
		Next
	End If
	
	Function LogNodesAndComponents(graphNodes:TGraphNode[], sccs:TGraphNode[][], builderDatas:TBuilderData[])
		Log ELogCategory.TypeCreation, "active nodes and edges:"
		For Local n:TGraphNode = EachIn graphNodes
			Local str:String
			Local builder:TTypeDeclarationSymbolBuilder = TTypeDeclarationSymbolBuilder(TBuilderData(n.obj).builder)
			str :+ builder.Declaration().Name() + " -> "
			Local estr:String[]
			For Local e:TGraphNode = EachIn n.edges
				Local eBuilder:TTypeDeclarationSymbolBuilder = TTypeDeclarationSymbolBuilder(TBuilderData(e.obj).builder)
				estr :+ [eBuilder.Declaration().Name()]
			Next
			str :+ ", ".Join(estr)
			Log ELogCategory.TypeCreation, str
		Next
		Log ELogCategory.TypeCreation, "sccs:"
		For Local scc:TGraphNode[] = EachIn sccs
			Local str:String
			For Local n:TGraphNode = EachIn scc
				Local builder:TTypeDeclarationSymbolBuilder = TTypeDeclarationSymbolBuilder(TBuilderData(n.obj).builder)
				Local nData:TBuilderData = TBuilderData.Find(builder.Declaration(), builderDatas)
				If str Then str :+ " <-> "
				str :+ builder.Declaration().Name()
				str :+ "(" + ["...", "finished"][nData.finished] + ", supers " + ["...", "finished"][nData.SuperTypesFinished(Null, builderDatas)] + ")"
			Next
			Log ELogCategory.TypeCreation, str
		Next
		Log ELogCategory.TypeCreation, "----------"
	End Function
	
	Function SetSuperType(builder:TTypeDeclarationSymbolBuilder, superTypeData:TSuperTypeData, bestMatch:ITypeDeclarationSymbol)
		If LoggingActive(ELogCategory.TypeCreation) Then Log ELogCategory.TypeCreation, builder.Declaration().Name() + ": setting super " + ["(?)", "class", "interface #" + superTypeData.superInterfaceIndex][superTypeData.isSuperClass + 2 * superTypeData.isSuperInterface] + " to " + bestMatch.Name()
		If TClassDeclarationSymbolBuilder(builder) Then
			Local builder:TClassDeclarationSymbolBuilder = TClassDeclarationSymbolBuilder(builder)
			Select True
				Case superTypeData.isSuperClass
					Assert IErrorTypeDeclarationSymbol(bestMatch) Or Not builder.Declaration().SuperClass() Else "Duplicate set"
					If IErrorTypeDeclarationSymbol(bestMatch) Then
						builder.SetSuperClass New TErrorTypeSymbol(superTypeData.typeSyntax)
					Else If IClassDeclarationSymbol(bestMatch) Then
						builder.SetSuperClass New TClassSymbol(superTypeData.typeSyntax, IClassDeclarationSymbol(bestMatch))
					Else
						ReportError bestMatch.NameWithTypeParameters() + " is " + WithIndefiniteArticle(bestMatch.KindName().ToLower()) + ", not a class", superTypeData.typeSyntax
						builder.SetSuperClass New TErrorTypeSymbol(superTypeData.typeSyntax)
					End If
				Case superTypeData.isSuperInterface
					Assert IErrorTypeDeclarationSymbol(bestMatch) Or Not builder.Declaration().SuperInterfaces()[superTypeData.superInterfaceIndex] Else "Duplicate set"
					If IErrorTypeDeclarationSymbol(bestMatch) Then
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TErrorTypeSymbol(superTypeData.typeSyntax)
					Else If IInterfaceDeclarationSymbol(bestMatch) Then
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TInterfaceSymbol(superTypeData.typeSyntax, IInterfaceDeclarationSymbol(bestMatch))
					Else
						ReportError bestMatch.NameWithTypeParameters() + " is " + WithIndefiniteArticle(bestMatch.KindName().ToLower()) + ", not an interface", superTypeData.typeSyntax
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TErrorTypeSymbol(superTypeData.typeSyntax)
					End If
				Default RuntimeError "Missing case"
			End Select
		Else If TStructDeclarationSymbolBuilder(builder) Then
			Local builder:TStructDeclarationSymbolBuilder = TStructDeclarationSymbolBuilder(builder)
			Select True
				Case superTypeData.isSuperInterface
					Assert IErrorTypeDeclarationSymbol(bestMatch) Or Not builder.Declaration().SuperInterfaces()[superTypeData.superInterfaceIndex] Else "Duplicate set"
					If IErrorTypeDeclarationSymbol(bestMatch) Then
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TErrorTypeSymbol(superTypeData.typeSyntax)
					Else If IInterfaceDeclarationSymbol(bestMatch) Then
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TInterfaceSymbol(superTypeData.typeSyntax, IInterfaceDeclarationSymbol(bestMatch))
					Else
						ReportError bestMatch.NameWithTypeParameters() + " is " + WithIndefiniteArticle(bestMatch.KindName().ToLower()) + ", not an interface", superTypeData.typeSyntax
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TErrorTypeSymbol(superTypeData.typeSyntax)
					End If
				Default RuntimeError "Missing case"
			End Select
		Else If TInterfaceDeclarationSymbolBuilder(builder) Then
			Local builder:TInterfaceDeclarationSymbolBuilder = TInterfaceDeclarationSymbolBuilder(builder)
			Select True
				Case superTypeData.isSuperInterface
					Assert IErrorTypeDeclarationSymbol(bestMatch) Or Not builder.Declaration().SuperInterfaces()[superTypeData.superInterfaceIndex] Else "Duplicate set"
					If IErrorTypeDeclarationSymbol(bestMatch) Then
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TErrorTypeSymbol(superTypeData.typeSyntax)
					Else If IInterfaceDeclarationSymbol(bestMatch) Then
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TInterfaceSymbol(superTypeData.typeSyntax, IInterfaceDeclarationSymbol(bestMatch))
					Else
						ReportError bestMatch.NameWithTypeParameters() + " is " + WithIndefiniteArticle(bestMatch.KindName().ToLower()) + ", not an interface", superTypeData.typeSyntax
						builder.SetSuperInterface superTypeData.superInterfaceIndex, New TErrorTypeSymbol(superTypeData.typeSyntax)
					End If
				Default RuntimeError "Missing case"
			End Select
		Else If TEnumDeclarationSymbolBuilder(builder) Then
			Local builder:TEnumDeclarationSymbolBuilder = TEnumDeclarationSymbolBuilder(builder)
			RuntimeError "TODO: enum super type?"
		Else
			RuntimeError "Missing case"
		End If
	End Function
	
	Function GetParentCodeBlockScope:TScope(syntax:ISyntax, scopeTree:TScopeTree)
		Assert TCodeBlockSyntax(syntax.Parent()) Else "Declaration is not in a block"
		Local scope:TScope = TScope(scopeTree.scopes[syntax.Parent()])
		Assert scope Else "Missing scope"
		Return scope
	End Function
	
	Function LookUpName:IDeclarationSymbol[](nameSyntax:TQualifiedNameSyntax, scope:TScope) ' will look up the name starting from this scope
		Assert nameSyntax.Parts() Else "Name is empty"
		If Not nameSyntax.Parts()[0].Identifier() Then RuntimeError "TODO: global scope lookup"
		For Local part:TQualifiedNamePartSyntax = EachIn nameSyntax.Parts()
			Local key:TSymbolKey = New TSymbolKey(part.Identifier().lexerToken.value)
			
		Next
	End Function
End Function
