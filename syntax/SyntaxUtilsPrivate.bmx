
? Debug
' verify types
Global ISyntaxDataOrSyntaxTokenTypeId:TTypeId = TTypeId.ForName("ISyntaxDataOrSyntaxToken")
Assert ISyntaxDataOrSyntaxTokenTypeId Else "ISyntaxDataOrSyntaxToken type not found"
Global ISyntaxDataTypeId:TTypeId = TTypeId.ForName("ISyntaxData")
Assert ISyntaxDataTypeId Else "ISyntaxData type not found"
For Local t:TTypeId = EachIn TTypeId.EnumTypes()
	If t.ExtendsType(ISyntaxDataTypeId) And Not t.IsAbstract() Then
		' verify presence of ISyntaxDataOrSyntaxToken field
		Local hasSyntaxDataOrSyntaxTokenField:Int = False
		For Local f:TField = EachIn t.EnumFields()
			If f.TypeId().ExtendsType(ISyntaxDataOrSyntaxTokenTypeId) Or (f.TypeId().ExtendsType(ArrayTypeId) And f.TypeId().ElementType().ExtendsType(ISyntaxDataOrSyntaxTokenTypeId)) Then
				hasSyntaxDataOrSyntaxTokenField = True
				Exit
			End If
		Next
		If Not hasSyntaxDataOrSyntaxTokenField Then RuntimeError "Type " + t.Name() + " which is non-abstract and implements " + ISyntaxDataTypeId.Name() + " must have at least one field of a type that implements " + ISyntaxDataOrSyntaxTokenTypeId.Name() + " (or an array of such a type)"
		' verify correct use of {nullable}
		For Local f:TField = EachIn t.EnumFields()
			If f.MetaData("nullable") Then
				If Not f.TypeId().ExtendsType(ObjectTypeId) Then RuntimeError "Cannot use {nullable} on non-object fields (" + t.Name() + "." + f.Name() + ")"
				If f.TypeId().ExtendsType(StringTypeId) Then RuntimeError "Cannot use {nullable} on String fields (" + t.Name() + "." + f.Name() + ")"
				If f.TypeId().ExtendsType(ArrayTypeId) Then RuntimeError "Cannot use {nullable} on array fields (" + t.Name() + "." + f.Name() + ")"
			End If
		Next
	End If
Next

Function Verify(syntaxData:ISyntaxData)
	Local t:TTypeId = TTypeId.ForObject(syntaxData)
	For Local f:TField = EachIn t.EnumFields()
		Local fValue:Object = f.Get(syntaxData)
		If Not fValue And Not f.TypeId().ExtendsType(ArrayTypeId) And Not TTypeId.ForObject(fValue) And Not f.MetaData("nullable") Then
			' check for {nullable} violations
			RuntimeError "Non-nullable field " + f.Name() + " in " + t.Name() + " instance contains Null value"
		Else If fValue And f.TypeId().ExtendsType(ArrayTypeId) And TTypeId.ForObject(fValue).Name() <> "Null[]" Then
			' check for null objects in arrays
			For Local i:Int = 0 Until f.TypeId().ArrayLength(fValue)
				Local element:Object = f.TypeId().GetArrayElement(fValue, i)
				If Not element And Not TTypeId.ForObject(element) Then
					RuntimeError "Array in field " + f.Name() + " in " + t.Name() + " instance contains Null object"
				End If
			Next
		End If
	Next
End Function
?

' this is very type-unsafe
Function ChildrenToArray:Object[](c1:Object = Null, c2:Object = Null, c3:Object = Null, c4:Object = Null, c5:Object = Null, c6:Object = Null, c7:Object = Null, c8:Object = Null, c9:Object = Null, c10:Object = Null, c11:Object = Null, c12:Object = Null)
	' filters out null objects and flattens arrays; returns array of the results
	
	Function GetCount:Int(c:Object)
		? Debug
		If TWeakReference(c) Then RuntimeError "Don't call this in types with weak refs"
		?
		Local cAsArray:Object[] = Object[](c)
		If cAsArray Then
			Local nonNullElements:Int = 0
			For Local i:Int = 0 Until cAsArray.length
				If ISyntax(cAsArray[i]) Or ISyntaxData(cAsArray[i]) Or TSyntaxToken(cAsArray[i]) Then nonNullElements :+ 1
			Next
			Return nonNullElements
		Else If ISyntax(c) Or ISyntaxData(c) Or TSyntaxToken(c) Then ' need this cast to filter out empty arrays
			Return 1
		Else
			Return 0
		End If
	End Function
	
	Function AddToArray(a:Object[], currentIndex:Int Var, c:Object)
		Local cAsArray:Object[] = Object[](c)
		If cAsArray Then
			For Local i:Int = 0 Until cAsArray.length
				If ISyntax(cAsArray[i]) Or ISyntaxData(cAsArray[i]) Or TSyntaxToken(cAsArray[i]) Then
					a[currentIndex] = cAsArray[i]
					currentIndex :+ 1
				End If
			Next
		Else If ISyntax(c) Or ISyntaxData(c) Or TSyntaxToken(c) Then
			a[currentIndex] = c
			currentIndex :+ 1
		Else
			' not an error, just won't show up in children
			'? Debug
			'If c And TTypeId.ForObject(c).Name() <> "Null[]" Then RuntimeError "Invalid child element type: " + TTypeId.ForObject(c).Name()
			'?
		End If
	End Function
	
	Local elementCount:Int = ..
		GetCount(c1) + ..
		GetCount(c2) + ..
		GetCount(c3) + ..
		GetCount(c4) + ..
		GetCount(c5) + ..
		GetCount(c6) + ..
		GetCount(c7) + ..
		GetCount(c8) + ..
		GetCount(c9) + ..
		GetCount(c10) + ..
		GetCount(c11) + ..
		GetCount(c12)
	
	Local children:Object[elementCount]
	Local currentIndex:Int = 0
	AddToArray children, currentIndex, c1
	AddToArray children, currentIndex, c2
	AddToArray children, currentIndex, c3
	AddToArray children, currentIndex, c4
	AddToArray children, currentIndex, c5
	AddToArray children, currentIndex, c6
	AddToArray children, currentIndex, c7
	AddToArray children, currentIndex, c8
	AddToArray children, currentIndex, c9
	AddToArray children, currentIndex, c10
	AddToArray children, currentIndex, c11
	AddToArray children, currentIndex, c12
	
	Return children
End Function
