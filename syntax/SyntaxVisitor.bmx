SuperStrict
Import "ISyntax.bmx"
Import "SyntaxLink.bmx"
Import BRL.Reflection
Import "ReflectionUtils.bmx"



Private
Global ISyntaxType:TTypeId = TTypeId.ForName("ISyntax")
Assert ISyntaxType Else "ISyntax type not found"
Global TSyntaxTokenType:TTypeId = TTypeId.ForName("TSyntaxToken")
Assert ISyntaxType Else "ISyntaxToken type not found"


Public

' WARNING: do not write visitor methods that accept an interface as their parameter type -
'          this doesn't work because reflection is broken

Type TSyntaxVisitor Abstract
	' implement VisitTopDown(node:...) and/or VisitBottomUp(node:...) methods in derived types
	' TODO: allow visit methods to return values which will be passed to other visit methods
	'       as a parameter (from parent, if top-down) or a map parameter (from children, if bottom-up)
End Type



Type TTreeRewritingSyntaxVisitor Extends TSyntaxVisitor Abstract
	' TODO: allow return values to replace nodes in the tree
	'       subsequent visit calls will be done on the replaced node:
	'       the previously returned node (if top-down) or an auto-generated new node with the
	'       original children having been replaced by previously returned nodes (if bottom-up)
End Type



Function VisitSyntax(visitor:TSyntaxVisitor, root:ISyntax)
	Const TopDown:Int = 1
	Const BottomUp:Int = 2
	Type TVisitMethodAndDirection ' TODO: struct
		Field ReadOnly visitMethod:TMethod
		Field ReadOnly direction:Int ' TODO: enum
		Method New(visitMethod:TMethod, direction:Int)
			Self.visitMethod = visitMethod
			Self.direction = direction
		End Method
	End Type
	
	' find all valid visit methods
	Local visitorType:TTypeId = TTypeId.ForObject(visitor)
	Local visitMethods:TVisitMethodAndDirection[]
	' TODO: BROKEN! this may randomly skip some of the methods because reflection is broken; this
	' seems to be random(!) per build (different results when the same code is compiled multiple times)
	For Local m:TMethod = EachIn visitorType._methodsList
		If m.Name() = "VisitTopDown" Or m.Name() = "VisitBottomUp" Then
			Local direction:Int
			Select m.Name()
				Case "VisitTopDown" direction = TopDown
				Case "VisitBottomUp" direction = BottomUp
			End Select
			Local returnType:TTypeId = m.TypeId().ReturnType()
			Local argTypes:TTypeId[] = m.ArgTypes()
			Assert argTypes[0] = ISyntaxType Or Not argTypes[0].IsInterface() Else "Invalid parameter type on " + visitorType.Name() + "." + m.Name() + " (can't use interfaces other than ISyntax due to reflection being broken)"
			Assert argTypes[0].ExtendsType(ISyntaxType) Or argTypes[0].Interfaces().Contains(ISyntaxType) Or ..
			       argTypes[0].ExtendsType(TSyntaxTokenType) Else "Invalid parameter type on " + visitorType.Name() + "." + m.Name()
			If returnType = VoidTypeId Then
				Assert argTypes.length = 1 Else "Invalid parameter count on " + visitorType.Name() + "." + m.Name()
			Else
				Assert returnType.ExtendsType(ObjectTypeId) Else "Invalid return type on " + visitorType.Name() + "." + m.Name() + " (must extend Object)"
				Assert argTypes.length = 2 Else "Invalid parameter count on " + visitorType.Name() + "." + m.Name()
				Select direction
					Case TopDown Assert argTypes[1] = returnType Else "Invalid parameter type on " + visitorType.Name() + "." + m.Name() + " (second parameter must match return type)"
					Case BottomUp Assert argTypes[1] = TTypeId.ForObject(New TMap) Else "Invalid parameter type on " + visitorType.Name() + "." + m.Name() + " (second parameter must be TMap)"
					Default RuntimeError "Missing case"
				End Select
			End If
			visitMethods :+ [New TVisitMethodAndDirection(m, direction)]
		End If
	Next
	Assert visitMethods Else "No VisitTopDown or VisitBottomUp method found in " + visitorType.Name()
	VisitSyntaxInner visitor, visitMethods, root
	
	Function VisitSyntaxInner(visitor:TSyntaxVisitor, visitMethods:TVisitMethodAndDirection[], nodeOrToken:ISyntaxOrSyntaxToken)
		Type TVisitMethodAndDirectionAndVisitor ' TODO: struct
			Field ReadOnly visitMethod:TMethod
			Field ReadOnly direction:Int ' TODO: enum
			Field ReadOnly visitor:TSyntaxVisitor
			Method New(visitMethod:TMethod, direction:Int, visitor:TSyntaxVisitor)
				Self.visitMethod = visitMethod
				Self.direction = direction
				Self.visitor = visitor
			End Method
		End Type
		
		' select visit methods applicable to this node
		Local nodeOrTokenType:TTypeId = TTypeId.ForObject(nodeOrToken)
		Local applicableMethods:TVisitMethodAndDirectionAndVisitor[]
		For Local v:TVisitMethodAndDirection = EachIn visitMethods
			Local visitMethod:TMethod = v.visitMethod
			Local direction:Int = v.direction
			Local argType:TTypeId = visitMethod.ArgTypes()[0]
			If nodeOrTokenType.ExtendsType(argType) Or nodeOrTokenType.Interfaces().Contains(argType) Then ' this is so broken
				applicableMethods :+ [New TVisitMethodAndDirectionAndVisitor(visitMethod, direction, visitor)]
			End If
		Next
		
		' visit nodes
		For Local m:TVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = TopDown Then
				Visit m.visitMethod, m.visitor, nodeOrToken
			End If
		Next
		
		If ISyntax(nodeOrToken) Then
			For Local nodeField:TField = EachIn GetAllFields(nodeOrTokenType)
				Local nodeFieldType:TTypeId = nodeField.TypeId()
				Local nodeFieldValue:Object = nodeField.Get(nodeOrToken)
				
				If ISyntaxOrSyntaxToken(nodeFieldValue) Then
					VisitSyntaxInner visitor, visitMethods, ISyntaxOrSyntaxToken(nodeFieldValue)
				Else If nodeFieldValue And ..
						nodeFieldType.ExtendsType(ArrayTypeId)..' And ..
						.. '(nodeFieldType.ElementType().ExtendsType(ISyntaxOrSyntaxTokenType) Or nodeFieldType.ElementType().Interfaces().Contains(ISyntaxOrSyntaxTokenType)) .. ' reflection is too broken for this, we will make do with a null check instead
				Then
					For Local i:Int = 0 Until nodeFieldType.ArrayLength(nodeFieldValue)
						Local nodeFieldArrayElement:ISyntaxOrSyntaxToken = ISyntaxOrSyntaxToken(nodeFieldType.GetArrayElement(nodeFieldValue, i))
						If Not nodeFieldArrayElement Then Continue ' this is that null check
						VisitSyntaxInner visitor, visitMethods, nodeFieldArrayElement
					Next
				End If
			Next
		End If
		
		For Local m:TVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = BottomUp Then
				Visit m.visitMethod, m.visitor, nodeOrToken
			End If
		Next
				
		Function Visit(visitMethod:TMethod, visitor:TSyntaxVisitor, nodeOrToken:ISyntaxOrSyntaxToken)
			'visitMethod.Invoke visitor, [node] ' .Invoke is broken
			Local fptr(v:Object, s:Object) = visitMethod._ref
			fptr visitor, nodeOrToken
		End Function
		Function Visit:Object(visitMethod:TMethod, visitor:TSyntaxVisitor, nodeOrToken:ISyntaxOrSyntaxToken, parentReturnValue:Object)
			'Return visitMethod.Invoke(visitor, [node, parentReturnValue]) ' .Invoke is broken
			Local fptr:Object(v:Object, s:Object, p:Object) = visitMethod._ref
			Return fptr(visitor, nodeOrToken, parentReturnValue)
		End Function
	End Function
End Function


