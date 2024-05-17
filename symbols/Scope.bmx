SuperStrict
Import "Symbols.bmx"
Import BRL.Map
Import "../util/Logging.bmx"



Type TScopeTree Final
	Field scopes:TMap = New TMap'<ISyntax, TScope>
	' TODO: where are scopes of constructed generic types or imported types linked?
	
	' TODO: lookup methods go here
	
	' TODO:
	' DO NOT USE SYNTAX TO NAVIGATE
	' syntax doesn't necessarily exist, e.g. for imported symbols
	' lookup should be symbol-based:
	' for reference, in roslyn:
	' - every symbol has a property to get its parent (containing) symbol
	' - every type symbol has a property to get its super type/interfaces
	' - every type or namespace symbol has a method to get its members, an overload that takes a string
	'   to look up, an overload to only get types (with/without the string) and an overload to only
	'   get types of a given arity
	' -> build a symbol tree containing parent/child relationships
	Method LookUpTypeDeclaration:IDeclarationSymbol[](startingScope:TScope, identifier:String, includeParentScopes:Int)', referenceLocation:???)
		Type TNoCancelCondition Implements ILookupCancelCondition Final
			Method LookupCancelCondition:Int(scope:TScope) Override Return False End Method
		End Type
		Global noCancelCondition:ILookupCancelCondition = New TNoCancelCondition
		Return LookUpTypeDeclaration(startingScope, identifier, includeParentScopes, noCancelCondition)
	End Method
	
	Method LookUpTypeDeclaration:IDeclarationSymbol[](startingScope:TScope, identifier:String, includeParentScopes:Int, cancelConditionObject:ILookupCancelCondition)', referenceLocation:???)
		' TODO: account for visibility
		' TODO: report error for locals if code location of declaration is after reference
		' ^ both of these will need referenceLocation
		'   alternatively: attach visibility to declarations and check locations of locals at use site
		' TODO: a "find only types" lookup is not feasible in BlitzMax because there are situations
		'       in which it isn't possible to distinguish between type and expression contexts,
		'       e.g. type casts; in these situations, a function would be able to shadow a type of
		'        the same name, so for consistency, this should be the case everywhere
		
		? Debug
			Local scopesContainsStartingScope:Int = False
			For Local s:TScope = EachIn scopes.Values()
				If s = startingScope Then scopesContainsStartingScope = True; Exit
			Next
			If Not scopesContainsStartingScope Then RuntimeError "Unknown scope"
		?
		If LoggingActive(ELogCategory.ScopeLookup) Then Log ELogCategory.ScopeLookup, "looking for " + identifier + " in scope " + startingScope.ToString()		
		
		' keep track of scopes that have already been checked to avoid checking the same scope twice
		Local visitedScopes:TScope[] = [startingScope]
		Function AlreadyVisited:Int(scope:TScope, visitedScopes:TScope[])
			For Local visitedScope:TScope = EachIn visitedScopes
				If scope = visitedScope Then Return True
			Next
			Return False
		End Function
		
		If cancelConditionObject.LookupCancelCondition(startingScope) Then Return Null

		Local declarations:IDeclarationSymbol[]
		
		' look up in starting scope
		Local key:TSymbolKey = New TSymbolKey(identifier)
		declarations = startingScope[key]
		
		' if this is a lookup in the body scope of a type, look up in super types
		Local typeDeclaration:ITypeDeclarationSymbol = FindTypeDeclarationWithScope(Self, startingScope)
		If typeDeclaration Then
			Local cancelled:Int = False
			declarations :+ LookUpInSuperTypes(Self, typeDeclaration, key, declarations, visitedScopes, cancelConditionObject, cancelled)
			If cancelled Then Return Null
		End If
		' when looking for a type, scopes of variables are excluded from the lookup
		' e.g. given "Type A Type B End Type End Type; Global x:A",
		' "Local y:A.B" is valid, but "Local y:x.B" is not
		' this is consistent with the fact that the parser does not allow arbitrary expressions there
		
		' if necessary, look up in parent (surrounding) scopes
		If includeParentScopes Then
			Local parentScope:TScope = startingScope.parent
			While parentScope
				If Not AlreadyVisited(parentScope, visitedScopes) Then
					declarations :+ WithoutOverriddenOrShadowedDeclarations(parentScope[key], declarations)
					visitedScopes :+ [parentScope]
				End If
				If LoggingActive(ELogCategory.ScopeLookup) Then Log ELogCategory.ScopeLookup, "looking in parent scope " + parentScope.ToString()
				Local parentScopeType:ITypeDeclarationSymbol = FindTypeDeclarationWithScope(Self, parentScope)
				If parentScopeType Then
					Local cancelled:Int = False
					declarations :+ LookUpInSuperTypes(Self, parentScopeType, key, declarations, visitedScopes, cancelConditionObject, cancelled)
					If cancelled Then Return Null
				End If
				parentScope = parentScope.parent
			Wend
		End If
		
		Return declarations
		
		Function LookUpInSuperTypes:IDeclarationSymbol[](Self_:TScopeTree, typeDeclaration:ITypeDeclarationSymbol, key:TSymbolKey, baseDeclarations:IDeclarationSymbol[], visitedScopes:TScope[] Var, cancelConditionObject:ILookupCancelCondition, cancelled:Int Var)
			Local resultDeclarationsInSuperTypes:IDeclarationSymbol[]
			
			For Local superType:ITypeSymbol = EachIn typeDeclaration.SuperTypes()
				' TODO: deal with generic or imported super types
				Local superTypeBodySyntax:ISyntax = superType.Declaration().BodySyntax()
				If IErrorTypeDeclarationSymbol(superType.Declaration()) Then Continue ' TODO
				Local superTypeScope:TScope = TScope(Self_.scopes[superTypeBodySyntax])
				
				If AlreadyVisited(superTypeScope, visitedScopes) Then Continue
				visitedScopes :+ [superTypeScope]
				If LoggingActive(ELogCategory.ScopeLookup) Then Log ELogCategory.ScopeLookup, "looking in super type " + typeDeclaration.Name() + "->" + superType.Declaration().Name() + " scope " + superTypeScope.ToString()
				
				If cancelConditionObject.LookupCancelCondition(superTypeScope) Then cancelled = True; Return Null
				
				Local declarationsInSuperType:IDeclarationSymbol[] = superTypeScope[key]
				If declarationsInSuperType Then 
					' filter out declarations that are overridden or shadowed
					' by declarations that have already been found
					declarationsInSuperType = WithoutOverriddenOrShadowedDeclarations(declarationsInSuperType, baseDeclarations)
					resultDeclarationsInSuperTypes :+ declarationsInSuperType
				End If
				resultDeclarationsInSuperTypes :+ LookUpInSuperTypes(Self_, superType.Declaration(), key, resultDeclarationsInSuperTypes + declarationsInSuperType, visitedScopes, cancelConditionObject, cancelled)
				If cancelled Then Return Null
			Next
			
			Return resultDeclarationsInSuperTypes
		End Function
		
		Function WithoutOverriddenOrShadowedDeclarations:IDeclarationSymbol[](declarations:IDeclarationSymbol[], baseDeclarations:IDeclarationSymbol[])
			If Not declarations Then Return []
			Local keep:Byte Ptr = StackAlloc SizeOf Byte Null * declarations.length
			Local resultDeclarationsCount:Int = declarations.length
			For Local d:Int = 0 Until declarations.length
				keep[d] = True
				Local declaration:IDeclarationSymbol = declarations[d]
				For Local baseDeclaration:IDeclarationSymbol = EachIn baseDeclarations
					If baseDeclaration = declaration Then
						' same
						keep[d] = False; resultDeclarationsCount :- 1; Exit
					ElseIf IVariableDeclarationSymbol(baseDeclaration) And IVariableDeclarationSymbol(declaration) Then
						If IOverloadableDeclarationSymbol(baseDeclaration) And IOverloadableDeclarationSymbol(declaration) Then
							' overloaded, overridden, shadowed?
							' TODO: keep or not?
							RuntimeError "TODO"
						Else
							' shadowed
							keep[d] = False; resultDeclarationsCount :- 1; Exit
						End If
					Else If ITypeDeclarationSymbol(baseDeclaration) And ITypeDeclarationSymbol(declaration) Then
						Local bTypeParameters:ITypeParameterDeclarationSymbol[] = ITypeDeclarationSymbol(baseDeclaration).TypeParameters()
						Local sTypeParameters:ITypeParameterDeclarationSymbol[] = ITypeDeclarationSymbol(declaration).TypeParameters()
						' TODO: type parameter defaults?
						If bTypeParameters.length = sTypeParameters.length Then
							' shadowed
							keep[d] = False; resultDeclarationsCount :- 1; Exit
						Else
							' overloaded
						End If
					Else
						' TODO: to prevent confusion, a single type should shadow an entire method group
						'       and a method should shadow an entire type group
						' TODO: keep or not?
						RuntimeError "TODO: other kind of declaration"
					End If
				Next
				If resultDeclarationsCount = 0 Then Exit
			Next
			
			If resultDeclarationsCount > 0 Then
				Local resultDeclarations:IDeclarationSymbol[resultDeclarationsCount]
				Local r:Int = 0
				For Local d:Int = 0 Until declarations.length
					If keep[d] Then resultDeclarations[r] = declarations[d]; r :+ 1
				Next
				Return resultDeclarations
			Else
				Return []
			End If
		End Function
		
		Function FindTypeDeclarationWithScope:ITypeDeclarationSymbol(Self_:TScopeTree, scope:TScope)'returns nullable
			' finds the type declaration that has the given scope as the scope of its body
			Local parentScope:TScope = scope.parent
			If Not parentScope Then Return Null
			For Local declaration:IDeclarationSymbol = EachIn parentScope
				If ITypeDeclarationSymbol(declaration) Then
					Local declaration:ITypeDeclarationSymbol = ITypeDeclarationSymbol(declaration)
					Assert declaration.BodySyntax() Else "Missing body syntax (TODO: type parameter?)"
					If Self_.scopes[declaration.BodySyntax()] = scope Then Return declaration
				End If
			Next
			Return Null
		End Function
	End Method
	
	Private
	Method FindTypeBodyScope:TScope(declaration:ITypeDeclarationSymbol)
		Assert TScope(scopes[declaration.BodySyntax()]) Else "Missing scope"
		Return TScope(scopes[declaration.BodySyntax()])
	End Method
End Type

Interface ILookupCancelCondition ' TODO: this should be nested, but that doesn't currently work in bcc
	Method LookupCancelCondition:Int(scope:TScope)
End Interface



Type TScope Final
	Field ReadOnly parent:TScope'nullable
	
	Private
	Field ReadOnly declarations:TMap = New TMap'<TSymbolKey, IDeclarationSymbol | IOverloadableDeclarationSymbol[]> ' key is not the same as name, since names are not case-sensitive and declarations can have non-referrable names
	' TODO: should non-referrable decls even be in scopes? for codegen?
	'       or does codegen just walk through symbol children and find declarations that way?
	'       generated code will not necessarily have the same scope structure as the original code anway
	'       but in that case, generated symbols will need to be inserted as children somewhere
	Field ReadOnly inheritsLocalsFromParent:Int
	
	Method New() End Method
	
	Public
	Method New(parent:TScope, inheritsLocalsFromParent:Int, hasMembersOf:IDeclarationWithMembersSymbol = Null)
		Self.parent = parent
		Self.inheritsLocalsFromParent = inheritsLocalsFromParent
	End Method
	
	Method AddDeclaration(declaration:IDeclarationSymbol)
		Local key:TSymbolKey = New TSymbolKey(declaration)
		If IOverloadableDeclarationSymbol(declaration) Then
			Local existingOverloads:IOverloadableDeclarationSymbol[] = IOverloadableDeclarationSymbol[](declarations[key])
			declarations[key] = existingOverloads + [declaration]
		Else
			Assert Not declarations.Contains(key) Else "Scope already contains a declaration with this key"
			declarations[key] = declaration
		End If
	End Method
	
	Method RemoveDeclaration(declaration:IDeclarationSymbol)
		Local key:TSymbolKey = New TSymbolKey(declaration)
		If IOverloadableDeclarationSymbol(declaration) Then
			Local existingOverloads:IOverloadableDeclarationSymbol[] = IOverloadableDeclarationSymbol[](declarations[key])
			For Local o:Int = 0 Until existingOverloads.length
				If existingOverloads[o] = declaration Then
					If existingOverloads.length = 1 Then
						declarations.Remove(key)
					Else
						declarations[key] = existingOverloads[..o] + existingOverloads[o + 1..]
					End If
					Return
				End If
			Next
			Assert False Else "Scope does not contain a declaration with this key"
		Else
			Assert declarations.Contains(key) Else "Scope does not contain a declaration with this key"
			declarations.Remove(key)
		End If
	End Method
	
	Method Operator []:IDeclarationSymbol[](key:TSymbolKey)
		Local value:Object = declarations[key]
		If value Then
			If IDeclarationSymbol(value) Then
				Return [IDeclarationSymbol(value)]'If Not (ILocalDeclarationSymbol(value) And Not includeLocals) Then Return [IDeclarationSymbol(value)]
			Else If IOverloadableDeclarationSymbol[](value) Then
				Return IOverloadableDeclarationSymbol[](value)
			Else
				RuntimeError "Missing case"
			End If
		Else
			Return Null'If parent Then Return parent.LookUpDeclarationWIP(key, includeLocals And inheritsLocalsFromParent) Else Return Null
		End If
	End Method
	
	Method ObjectEnumerator:TScopeEnumerator() ' returns declarations, not keys
		Return New TScopeEnumerator(declarations)
	End Method
	
	Rem
	Method LookUpDeclaration:IDeclaration[](key:SSymbolKey, includeLocals:Int)
		Local value:Object = declarations[key.ToString()]
		If value Then
			If IDeclaration(value) Then
				If Not (TLocalDeclaration(value) And Not includeLocals) Then Return [IDeclaration(value)]
			Else If IOverloadableDeclaration[](value) Then
				Return IOverloadableDeclaration[](value)
			Else
				RuntimeError "Missing case"
			End If
		Else
			If parent Then Return parent.LookUpDeclaration(key, includeLocals And inheritsLocalsFromParent) Else Return Null
		End If
	End Method
	End Rem
	
	Method ToString:String() Override
		Local str:String
		For Local o:Object = EachIn declarations.Values()
			If IDeclarationSymbol(o) Then
				str = Append(str, IDeclarationSymbol(o))
			Else If IOverloadableDeclarationSymbol[](o) Then
				Local overloads:IOverloadableDeclarationSymbol[] = IOverloadableDeclarationSymbol[](o)
				str = Append(str, overloads[0])
				If overloads.length > 1 Then str :+ " (*" + IOverloadableDeclarationSymbol[](o).length + ")"
			Else
				RuntimeError "Missing case"
			End If
		Next
		Return "Scope [" + str + "]"
		
		Function Append:String(str:String, declaration:IDeclarationSymbol)
			If str Then str :+ ", "
			str :+ declaration.Name()'.ToString()
			Return str
		End Function
	End Method
End Type

Type TScopeEnumerator Final
	Field ReadOnly declarationsEnumerator:TNodeEnumerator
	Field currentArray:IDeclarationSymbol[] = Null
	Field nextIndex:Int = 0
	
	Method New(declarations:TMap)
		 Self.declarationsEnumerator = declarations.ObjectEnumerator()
	End Method
	
	Method HasNext:Int()
		Return (currentArray And nextIndex < currentArray.length) Or declarationsEnumerator.HasNext()
	End Method
	
	Method NextObject:Object()
		If currentArray And nextIndex < currentArray.length Then
			nextIndex :+ 1
			Return currentArray[nextIndex - 1]
		Else
			Repeat
				Local kv:TKeyValue = TKeyValue(declarationsEnumerator.NextObject())
				If Not kv Then
					Return Null
				Else If IDeclarationSymbol(kv.Value()) Then
					Return kv.Value()
				Else If IDeclarationSymbol[](kv.Value()) Then
					currentArray = IDeclarationSymbol[](kv.Value())
					If currentArray.length > 0 Then nextIndex = 1; Return currentArray[0] Else nextIndex = 0
				Else
					RuntimeError "Missing case"
				End If
			Forever
		End If
	End Method
End Type



Type TSymbolKey Final
	Private
	Field ReadOnly name:String
	
	Method New() End Method
	
	Public
	Method New(declaration:IDeclarationSymbol)
		' TODO: support overloads
		If declaration.IsReferrableByName() Then
			Self.name = declaration.Name().ToLower() ' could trade speed for memory by moving the ToLower to the comparison instead
		Else
			RuntimeError "TODO"'Else name = "~~" + declaration.GetName().ToLower()
		End If
	End Method
	
	Method New(name:String)
		Self.name = name.ToLower() ' see above
	End Method
	
	Method Compare:Int(other:Object) Override
		If TSymbolKey(other) Then
			Return Self.Compare_(TSymbolKey(other))
		Else
			RuntimeError "no"
		End If
	End Method
	
	Method Compare_:Int(other:TSymbolKey)
		Return Self.name.Compare(other.name)
	End Method
	
	'Method ToString:String() Override
	'	Return str
	'End Method
End Type
