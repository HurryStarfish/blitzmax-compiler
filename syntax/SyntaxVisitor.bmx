SuperStrict
Import "SyntaxBase.bmx"
Import BRL.Reflection
Import "../util/ReflectionUtils.bmx"



Private
Const TopDownVisitMethodName:String = "VisitTopDown"
Const BottomUpVisitMethodName:String = "VisitBottomUp"

Global ISyntaxOrSyntaxTokenTypeId:TTypeId = TTypeId.ForName("ISyntaxOrSyntaxToken")
Assert ISyntaxOrSyntaxTokenTypeId Else "ISyntaxOrSyntaxToken type not found"



Public
' This class can be inherited to create a syntax visitor.
' Syntax visitors can define "VisitTopDown" and/or "VisitBottomUp" visitor methods to visit
' nodes in the syntax tree. Such visitor methods must have a first parameter of type
' ISyntaxOrSyntaxToken or a subtype thereof, which acts as a filter to specify which nodes of
' the tree the method should be called on.
' Optionally, visitor methods can have a return type and a corresponding second parameter of a
' type that is assignable from the return type (for top-down methods) or an array of such a type
' (for bottom-up methods). This allows them to pass data between invocations.
' A {DataGroup=...} meta data entry can be added to such methods to specify which other methods,
' in addition to themselves, they will share data with. Methods that have the same data group will
' pass data to each other and must have non-overlapping types for their first parameter (i.e.
' they must handle different types of nodes). A method without a specified DataGroup is equivalent
' to having {DataGroup=""}.
Type TSyntaxVisitor Abstract
	Method Visit(tree:TSyntaxTree) Final
		VisitSyntax Self, tree.GetRootSyntax()
	End Method
End Type


Rem
Type TTreeRewritingSyntaxVisitor Extends TSyntaxVisitor Abstract
	' TODO: allow return values to replace nodes in the tree
	'       subsequent visit calls will be done on the replaced node:
	'       the previously returned node (if top-down) or an auto-generated new node with the
	'       original children having been replaced by previously returned nodes (if bottom-up)
End Type
End Rem

Private
Global VisitMethods:TMap = New TMap'<TTypeId, SVisitMethodData[]>



Enum EVisitDirection:Byte
	TopDown
	BottomUp
End Enum

Struct SVisitMethodData
	Field ReadOnly visitMethod:TMethod
	Field ReadOnly direction:EVisitDirection
	Field ReadOnly hasDataParameter:Int
	Field ReadOnly dataGroup:String
	
	Method New(visitMethod:TMethod, direction:EVisitDirection, dataGroup:String)
		Self.visitMethod = visitMethod
		Self.direction = direction
		Self.hasDataParameter = visitMethod.ArgTypes().length >= 2
		Self.dataGroup = dataGroup
	End Method
End Struct



Function GetVisitMethods:SVisitMethodData[](visitorType:TTypeId)
	Local visitMethods:SVisitMethodData[]
	For Local m:TMethod = EachIn visitorType.EnumMethods()
		Local direction:EVisitDirection
		Select m.Name()
			Case TopDownVisitMethodName direction = EVisitDirection.TopDown
			Case BottomUpVisitMethodName direction = EVisitDirection.BottomUp
			Default Continue
		End Select
		Local returnType:TTypeId = m.TypeId().ReturnType()
		Local argTypes:TTypeId[] = m.ArgTypes()
		Local dataGroup:String = m.MetaData("DataGroup")
		? Debug
			Assert argTypes.length = 1 Or argTypes.length = 2 Else "Invalid parameter count on " + visitorType.Name() + "." + m.Name()
			Assert argTypes[0].ExtendsType(ISyntaxOrSyntaxTokenTypeId) Else "Invalid first parameter type (must be a subtype of " + ISyntaxOrSyntaxTokenTypeId.Name() + ") on " + visitorType.Name() + "." + m.Name()
			If returnType = VoidTypeId And argTypes.length = 1 Then
				Assert Not dataGroup Else "DataGroup specified but no data parameter on " + visitorType.Name() + "." + m.Name() 
			Else If argTypes.length = 2 Then
				Select direction
					Case EVisitDirection.TopDown
						Assert returnType.ExtendsType(ObjectTypeId) Else "Invalid return type (must be void or an object type) on " + visitorType.Name() + "." + m.Name()
						Assert argTypes[1].ExtendsType(ObjectTypeId) Else "Invalid second parameter type (must be an object type) on " + visitorType.Name() + "." + m.Name()
						Assert returnType.ExtendsType(argTypes[1]) Else "Return type and second parameter type do not match on " + visitorType.Name() + "." + m.Name()
						For Local other:SVisitMethodData = EachIn visitMethods
							If other.direction = direction And other.hasDataParameter And other.dataGroup = dataGroup Then
								Assert Not (argTypes[0].ExtendsType(other.visitMethod.ArgTypes()[0]) Or other.visitMethod.ArgTypes()[0].ExtendsType(argTypes[0])) Else "First parameter type overlaps between " + visitorType.Name() + "." + m.Name() + " overloads in " + ["default data group", "data group ~q" + dataGroup + "~q"][dataGroup <> ""]
								Assert returnType.ExtendsType(other.visitMethod.ArgTypes()[1]) Else "Return type and second parameter type do not match between " + visitorType.Name() + "." + m.Name() + " overloads in " + ["default data group", "data group ~q" + dataGroup + "~q"][dataGroup <> ""]
								Assert other.visitMethod.ReturnType().ExtendsType(argTypes[1]) Else "Return type and second parameter type do not match between " + visitorType.Name() + "." + m.Name() + " overloads in " + ["default data group", "data group ~q" + dataGroup + "~q"][dataGroup <> ""]
							End If
						Next
					Case EVisitDirection.BottomUp
						Assert returnType.ExtendsType(ObjectTypeId) Else "Invalid return type (must be void or an object type) on " + visitorType.Name() + "." + m.Name()
						Assert argTypes[1].ExtendsType(ArrayTypeId) Else "Invalid second parameter type (must be an array type) on " + visitorType.Name() + "." + m.Name()
						Assert argTypes[1].Dimensions() = 1 Else "Invalid second parameter type dimensions on " + visitorType.Name() + "." + m.Name()
						Assert argTypes[1].ElementType().ExtendsType(ObjectTypeId) Else "Invalid second parameter type (element type must be an object type) on " + visitorType.Name() + "." + m.Name()
						Assert returnType.ExtendsType(argTypes[1].ElementType()) Else "Return type and second parameter type do not match on " + visitorType.Name() + "." + m.Name()
						For Local other:SVisitMethodData = EachIn visitMethods
							If other.direction = direction And other.hasDataParameter And other.dataGroup = dataGroup Then
								Assert Not (argTypes[0].ExtendsType(other.visitMethod.ArgTypes()[0]) Or other.visitMethod.ArgTypes()[0].ExtendsType(argTypes[0])) Else "First parameter type overlaps between " + visitorType.Name() + "." + m.Name() + " overloads in " + ["default data group", "data group ~q" + dataGroup + "~q"][dataGroup <> ""]
								Assert returnType.ExtendsType(other.visitMethod.ArgTypes()[1].ElementType()) Else "Return type and second parameter type do not match between " + visitorType.Name() + "." + m.Name() + " overloads in " + ["default data group", "data group ~q" + dataGroup + "~q"][dataGroup <> ""]
								Assert other.visitMethod.ReturnType().ExtendsType(argTypes[1].ElementType()) Else "Return type and second parameter type do not match between " + visitorType.Name() + "." + m.Name() + " overloads in " + ["default data group", "data group ~q" + dataGroup + "~q"][dataGroup <> ""]
							End If
						Next
					Default RuntimeError "Missing case"
				End Select
			Else
				RuntimeError "Return type and parameter types do not match on " + visitorType.Name() + "." + m.Name()
			End If
		?
		visitMethods :+ [New SVisitMethodData(m, direction, dataGroup)]
	Next
	Assert visitMethods Else "No " + TopDownVisitMethodName + " or " + BottomUpVisitMethodName + " method found in " + visitorType.Name()
	Return visitMethods
End Function



Function VisitSyntax(visitor:TSyntaxVisitor, rootSyntax:ISyntax)
	Local map:TMap = VisitMethods ' because ".VisitMethods" is broken
	Local visitorType:TTypeId = TTypeId.ForObject(visitor)
	Local visitMethods:SVisitMethodData[] = SVisitMethodData[](map[visitorType])
	If Not visitMethods Then
		visitMethods = GetVisitMethods(visitorType)
		map[visitorType] = visitMethods
	End If
	Local dataGroups:String[]
	#VisitMethodLoop
	For Local m:Int = 0 Until visitMethods.length
		If Not visitMethods[m].hasDataParameter Then Continue VisitMethodLoop
		For Local d:String = EachIn dataGroups
			If d = visitMethods[m].dataGroup Then Continue VisitMethodLoop
		Next
		dataGroups :+ [visitMethods[m].dataGroup]
	Next
	
	VisitSyntaxInner visitor, visitMethods, rootSyntax, dataGroups, New Object[dataGroups.length]
	
	Function VisitSyntaxInner:Object[][](visitor:TSyntaxVisitor, visitMethods:SVisitMethodData[], syntaxOrSyntaxToken:ISyntaxOrSyntaxToken, dataGroups:String[], topDownData:Object[])
		' select visit methods applicable to this node
		Local nodeOrTokenType:TTypeId = TTypeId.ForObject(syntaxOrSyntaxToken)
		Type TNotApplicableData Final End Type
		Global NotApplicableData:TNotApplicableData = New TNotApplicableData
		Local applicable:Byte Ptr = StackAlloc SizeOf Byte Null * visitMethods.length
		For Local m:Int = 0 Until visitMethods.length
			applicable[m] = False
			Local syntaxArgType:TTypeId = visitMethods[m].visitMethod.ArgTypes()[0]
			If nodeOrTokenType.ExtendsType(syntaxArgType) Or nodeOrTokenType.Interfaces().Contains(syntaxArgType) Then ' this is so broken
				applicable[m] = True
			End If
		Next
		Local topDownDataInner:Object[] = topDownData[..]
		Local bottomUpDataInner:Object[][dataGroups.length]
		'Local bottomUpData:Object[][dataGroups.length]
		'For Local d:Int = 0 Until dataGroups.length bottomUpData[d] = NotApplicableData Next
		
		' top-down visit
		For Local m:Int = 0 Until visitMethods.length
			If applicable[m] And visitMethods[m].direction = EVisitDirection.TopDown Then
				If visitMethods[m].hasDataParameter Then
					Local dataGroupIndex:Int = GetDataGroupIndex(dataGroups, visitMethods[m])
					topDownDataInner[dataGroupIndex] = Visit(visitMethods[m].visitMethod, visitor, syntaxOrSyntaxToken, topDownData[dataGroupIndex])
					' different visit methods in the same data group may not handle the same nodes,
					' so no attempt is made to check whether return values overwrite each other here
				Else
					Visit visitMethods[m].visitMethod, visitor, syntaxOrSyntaxToken
				End If
			End If
		Next
		
		' recursive descent
		If ISyntax(syntaxOrSyntaxToken) Then
			Local children:ISyntaxOrSyntaxToken[] = ISyntax(syntaxOrSyntaxToken).GetChildren()
			For Local c:Int = 0 Until children.length
				Local bottomUpDataInnerForChild:Object[][] = VisitSyntaxInner(visitor, visitMethods, children[c], dataGroups, topDownDataInner)
				' ^ from each call comes either a return value (if the child was visited by a method)
				'   or an array (if it wasn't but children of that child were); since this applies
				'   recursively, the array can be mixed too
				Assert bottomUpDataInnerForChild.length = dataGroups.length Else "Wrong length"
				For Local d:Int = 0 Until dataGroups.length
					'If bottomUpDataInnerForChild[d] <> NotApplicableData Then bottomUpDataInner[d] :+ [bottomUpDataInnerForChild[d]]
					bottomUpDataInner[d] :+ bottomUpDataInnerForChild[d]
				Next
			Next
		End If
		
		' bottom-up visit
		For Local m:Int = 0 Until visitMethods.length
			If applicable[m] And visitMethods[m].direction = EVisitDirection.BottomUp Then
				If visitMethods[m].hasDataParameter Then
					Local dataGroupIndex:Int = GetDataGroupIndex(dataGroups, visitMethods[m])
					'bottomUpData[dataGroupIndex] = [Visit(visitMethods[m].visitMethod, visitor, syntaxOrSyntaxToken, bottomUpDataInner[dataGroupIndex])]
					bottomUpDataInner[dataGroupIndex] = [Visit(visitMethods[m].visitMethod, visitor, syntaxOrSyntaxToken, bottomUpDataInner[dataGroupIndex])]
					' different visit methods in the same data group must not handle the same nodes,
					' so no attempt is made to check whether return values overwrite each other here
				Else
					Visit visitMethods[m].visitMethod, visitor, syntaxOrSyntaxToken
				End If
			End If
		Next
		
		' where not applicable, passout inner instead
		'Return bottomUpData
		Return bottomUpDataInner
		
		Function GetDataGroupIndex:Int(dataGroups:String[], visitMethod:SVisitMethodData)
			For Local d:Int = 0 Until dataGroups.length
				If dataGroups[d] = visitMethod.dataGroup Then Return d
			Next
			RuntimeError "Unknown data group"
		End Function
		
		Function Visit(visitMethod:TMethod, visitor:TSyntaxVisitor, nodeOrToken:ISyntaxOrSyntaxToken)
			visitMethod.Invoke visitor, [nodeOrToken]
		End Function
		Function Visit:Object(visitMethod:TMethod, visitor:TSyntaxVisitor, nodeOrToken:ISyntaxOrSyntaxToken, visitMethodDataParameter:Object)
			If Not visitMethodDataParameter And visitMethod.ArgTypes()[1] = StringTypeId Then visitMethodDataParameter = "" ' aaaaaaaaaaaaa
			Return visitMethod.Invoke(visitor, [nodeOrToken, visitMethodDataParameter])
		End Function
	End Function
End Function


