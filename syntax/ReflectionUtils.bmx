SuperStrict
Import BRL.Reflection



Function GetAllFields:TList(t:TTypeId)
	If t = ObjectTypeId Then
		Return New TList
	Else
		Local fields:TList = GetAllFields(t.SuperType())
		For Local f:TField = EachIn t._fieldsList
			fields.AddLast f
		Next
		Return fields
	End If
End Function
