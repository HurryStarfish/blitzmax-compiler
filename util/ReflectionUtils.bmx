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



Function ExtendsOrImplements:Int(t1:TTypeId, t2:TTypeId)
	If t1 = t2 Then Return True
	If t1.ExtendsType(t2) Then Return True
	Local interfaces:TList = t1.Interfaces()
	If interfaces Then
		For Local i:TTypeId = EachIn interfaces
			If ExtendsOrImplements(i, t2) Then Return True
		Next
	End If
	Return False
End Function
