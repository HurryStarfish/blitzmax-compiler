SuperStrict
Import "Symbols.bmx"
Import BRL.Map



Type TScope Final
	Private
	Field ReadOnly declarations:TMap = New TMap'<String, IDeclaration | IOverloadableDeclaration[]> ' key is not the same as name
	Field ReadOnly parent:TScope
	Field ReadOnly inheritsLocalsFromParent:Int
	
	Method New() End Method
	
	Public
	Method New(parent:TScope, inheritsLocalsFromParent:Int)
		Self.parent = parent
		Self.inheritsLocalsFromParent = inheritsLocalsFromParent
	End Method
	
	Method AddDeclaration(declaration:IDeclaration)
		Local keyStr:String = New SSymbolKey(declaration).ToString()
		If IOverloadableDeclaration(declaration) Then
			Local existingOverloads:IOverloadableDeclaration[] = IOverloadableDeclaration[](declarations[keyStr])
			declarations[keyStr] = existingOverloads + [declaration]
		Else
			Assert Not declarations.Contains(keyStr) Else "Scope already contains a declaration with this key"
			declarations[keyStr] = declaration
		End If
	End Method
	
	Method RemoveDeclaration(declaration:IDeclaration)
		Local keyStr:String = New SSymbolKey(declaration).ToString()
		If IOverloadableDeclaration(declaration) Then
			Local existingOverloads:IOverloadableDeclaration[] = IOverloadableDeclaration[](declarations[keyStr])
			For Local o:Int = 0 Until existingOverloads.length
				If existingOverloads[o] = declaration Then
					If existingOverloads.length = 1 Then
						declarations.Remove(keyStr)
					Else
						declarations[keyStr] = existingOverloads[..o] + existingOverloads[o + 1..]
					End If
					Return
				End If
			Next
			Assert False Else "Scope does not contain a declaration with this key"
		Else
			Assert declarations.Contains(keyStr) Else "Scope does not contain a declaration with this key"
			declarations.Remove(keyStr)
		End If
	End Method
	
	Method LookUpDeclarationWIP:IDeclaration[](keyStr:String, includeLocals:Int)
		Local value:Object = declarations[keyStr]
		If value Then
			If IDeclaration(value) Then
				If Not (TLocalDeclaration(value) And Not includeLocals) Then Return [IDeclaration(value)]
			Else If IOverloadableDeclaration[](value) Then
				Return IOverloadableDeclaration[](value)
			Else
				RuntimeError "Missing case"
			End If
		Else
			If parent Then Return parent.LookUpDeclarationWIP(keyStr, includeLocals And inheritsLocalsFromParent) Else Return Null
		End If
	End Method
	
	'Method GetSymbolForName:TNamedSymbol(name:String) ' returns null if not found
	'	Return TNamedSymbol(symbols[(name)])
	'End Method
	Rem
	Method LookUpDeclaration:IDeclaration[](key:SSymbolKey, referenceLocation:???)
		' TODO: account for visibility
		' TODO: report error for locals if code location of declaration is after reference
		' ^ both of these need referenceLocation
		Return LookUpDeclaration(key, )
	End Method
	
	Private
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
	Public
	Method ToString:String() Override
		Local str:String
		For Local o:Object = EachIn declarations.Values()
			If IDeclaration(o) Then
				str = Append(str, IDeclaration(o))
			Else If IOverloadableDeclaration[](o) Then
				Local overloads:IOverloadableDeclaration[] = IOverloadableDeclaration[](o)
				str = Append(str, overloads[0])
				If overloads.length > 1 Then str :+ " (*" + IOverloadableDeclaration[](o).length + ")"
			Else
				RuntimeError "Missing case"
			End If
		Next
		Return "Scope [" + str + "]"
		
		Function Append:String(str:String, declaration:IDeclaration)
			If str Then str :+ ", "
			str :+ declaration.ToString()
			Return str
		End Function
	End Method
End Type



Struct SSymbolKey
	Private
	Field ReadOnly str:String
	
	Public
	Method New(declaration:IDeclaration)
		' TODO: support overloads
		If declaration.IsReferrableByName() Then str = declaration.GetName().ToLower() Else str = "~~" + declaration.GetName().ToLower()
	End Method
	
	Method ToString:String()
		Return str
	End Method
End Struct
