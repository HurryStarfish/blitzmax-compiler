SuperStrict
Import "ISyntax.bmx"
Import BRL.Reflection
Import "ReflectionUtils.bmx"



Private
Global iSyntaxType:TTypeId = TTypeId.ForName("ISyntax")
Assert iSyntaxType Else "ISyntax type not found"



Public

' WARNING: do not write visitor methods that accept an interface as their parameter type -
'          this doesn't work because reflection is broken

Type TSyntaxVisitor Abstract
	' implement VisitTopDown(node:...) and/or VisitBottomUp(node:...) methods in derived types
	' TODO: encode top-down/bottom-up in name
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
	Local visitMethods:TVisitMethodAndDirection[]' = visitorType.FindMethod("Visit")
	For Local m:TMethod = EachIn visitorType._methodsList
		If m.Name() = "VisitTopDown" Or m.Name() = "VisitBottomUp" Then
			Local direction:Int
			Select m.Name()
				Case "VisitTopDown" direction = TopDown
				Case "VisitBottomUp" direction = BottomUp
			End Select
			Local returnType:TTypeId = m.TypeId().ReturnType()
			Local argTypes:TTypeId[] = m.ArgTypes()
			Assert returnType = VoidTypeId Else "Invalid return type on " + visitorType.Name() + "." + m.Name()
			Assert argTypes.length = 1 Else "Invalid parameter count on " + visitorType.Name() + "." + m.Name()
			Assert argTypes[0] = iSyntaxType Or Not argTypes[0].IsInterface() Else "Invalid parameter type on " + visitorType.Name() + "." + m.Name() + " (can't use interfaces other than ISyntax due to reflection being broken)"
			Assert argTypes[0].ExtendsType(iSyntaxType) Or argTypes[0].Interfaces().Contains(iSyntaxType) Else "Invalid parameter type on " + visitorType.Name() + "." + m.Name()
			visitMethods :+ [New TVisitMethodAndDirection(m, direction)]
		End If
	Next
	Assert visitMethods Else "No VisitTopDown or VisitBottomUp method found in " + visitorType.Name()
	VisitSyntaxInner visitor, visitMethods, root
	
	Function VisitSyntaxInner(visitor:TSyntaxVisitor, visitMethods:TVisitMethodAndDirection[], node:ISyntax)
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
		Local nodeType:TTypeId = TTypeId.ForObject(node)
		Local applicableMethods:TVisitMethodAndDirectionAndVisitor[]
		For Local v:TVisitMethodAndDirection = EachIn visitMethods
			Local visitMethod:TMethod = v.visitMethod
			Local direction:Int = v.direction
			Local argType:TTypeId = visitMethod.ArgTypes()[0]
			If nodeType.ExtendsType(argType) Or nodeType.Interfaces().Contains(argType) Then ' this is so broken
				applicableMethods :+ [New TVisitMethodAndDirectionAndVisitor(visitMethod, direction, visitor)]
			End If
		Next
		
		' visit nodes
		For Local m:TVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = TopDown Then Visit m.visitMethod, m.visitor, node
		Next
		
		For Local nodeField:TField = EachIn GetAllFields(nodeType)
			Local nodeFieldType:TTypeId = nodeField.TypeId()
			Local nodeFieldValue:Object = nodeField.Get(node)
			
			If ISyntax(nodeFieldValue) Then
				VisitSyntaxInner visitor, visitMethods, ISyntax(nodeFieldValue)
			Else If nodeFieldValue And ..
			        nodeFieldType.ExtendsType(ArrayTypeId)..' And ..
			        .. '(nodeFieldType.ElementType().ExtendsType(iSyntaxType) Or nodeFieldType.ElementType().Interfaces().Contains(iSyntaxType)) .. ' reflection is too broken for this, we will make do with a null check instead
			Then
				For Local i:Int = 0 Until nodeFieldType.ArrayLength(nodeFieldValue)
					Local nodeFieldArrayElement:ISyntax = ISyntax(nodeFieldType.GetArrayElement(nodeFieldValue, i))
					If Not nodeFieldArrayElement Then Continue ' this is that null check
					VisitSyntaxInner visitor, visitMethods, nodeFieldArrayElement
				Next
			End If
		Next
		
		For Local m:TVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = BottomUp Then Visit m.visitMethod, m.visitor, node
		Next
				
		Function Visit(visitMethod:TMethod, visitor:TSyntaxVisitor, node:ISyntax)
			'visitMethod.Invoke visitor, [node] ' .Invoke is broken
			Local fptr(v:Object, s:Object) = visitMethod._ref
			fptr visitor, node
		End Function
	End Function
End Function


