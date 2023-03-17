SuperStrict
Import "ISyntax.bmx"
Import "SyntaxTree.bmx"
Import BRL.Reflection
Import "ReflectionUtils.bmx"



Private
Global ISyntaxType:TTypeId = TTypeId.ForName("ISyntax")
Global TSyntaxTokenType:TTypeId = TTypeId.ForName("TSyntaxToken")
Global TSyntaxLinkType:TTypeId = TTypeId.ForName("TSyntaxLink")
Assert ISyntaxType Else "ISyntax type not found"
Assert TSyntaxTokenType Else "ISyntaxToken type not found"
Assert TSyntaxLinkType Else "TSyntaxLink type not found"


Public

' WARNING: do not write visitor methods that accept an interface as their parameter type -
'          this doesn't work because reflection is broken

Type TSyntaxVisitor Abstract
	' implement VisitTopDown(node:...) and/or VisitBottomUp(node:...) methods in derived types
	' TODO: allow visit methods to return values which will be passed to other visit methods
	'       as a parameter (from parent, if top-down) or a map parameter (from children, if bottom-up)
	
	Method Visit(tree:TSyntaxTree) Final
		VisitSyntax Self, tree.GetRoot()
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

Function VisitSyntax(visitor:TSyntaxVisitor, rootLink:TSyntaxLink)
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
			Assert returnType = VoidTypeId Else "Invalid return type on " + visitorType.Name() + "." + m.Name()
			Assert argTypes.length = 2 Else "Invalid parameter count on " + visitorType.Name() + "." + m.Name()
			Assert argTypes[0].ExtendsType(TSyntaxLinkType) Else "Invalid 1st parameter type on " + visitorType.Name() + "." + m.Name() + " (must be TSyntaxLink)"
			Assert argTypes[1] = ISyntaxType Or Not argTypes[0].IsInterface() Else "Invalid 2nd parameter type on " + visitorType.Name() + "." + m.Name() + " (must be a subtype of ISyntaxOrSyntaxToken, can't use interfaces other than ISyntax due to reflection being broken)"
			Assert argTypes[1].ExtendsType(ISyntaxType) Or argTypes[1].Interfaces().Contains(ISyntaxType) Or ..
			       argTypes[1].ExtendsType(TSyntaxTokenType) Else "Invalid 2nd parameter type on " + visitorType.Name() + "." + m.Name()
			visitMethods :+ [New TVisitMethodAndDirection(m, direction)]
		End If
	Next
	Assert visitMethods Else "No VisitTopDown or VisitBottomUp method found in " + visitorType.Name()
	
	VisitSyntaxInner visitor, visitMethods, rootLink, rootLink.GetSyntaxOrSyntaxToken()
	
	Function VisitSyntaxInner(visitor:TSyntaxVisitor, visitMethods:TVisitMethodAndDirection[], link:TSyntaxLink, nodeOrToken:ISyntaxOrSyntaxToken)
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
			Local syntaxArgType:TTypeId = visitMethod.ArgTypes()[1]
			If nodeOrTokenType.ExtendsType(syntaxArgType) Or nodeOrTokenType.Interfaces().Contains(syntaxArgType) Then ' this is so broken
				applicableMethods :+ [New TVisitMethodAndDirectionAndVisitor(visitMethod, direction, visitor)]
			End If
		Next
		
		' top-down visit
		For Local m:TVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = TopDown Then
				Visit m.visitMethod, m.visitor, link, nodeOrToken
			End If
		Next
		
		For Local childLink:TSyntaxLink = EachIn link.GetChildren()
			VisitSyntaxInner visitor, visitMethods, childLink, childLink.GetSyntaxOrSyntaxToken()
		Next
			Rem
		If ISyntax(nodeOrToken) Then
			For Local nodeField:TField = EachIn GetAllFields(nodeOrTokenType)
				Local nodeFieldType:TTypeId = nodeField.TypeId()
				Local nodeFieldValue:Object = nodeField.Get(nodeOrToken)
				
				If ISyntaxOrSyntaxToken(nodeFieldValue) Then
					VisitSyntaxInner visitor, visitMethods, , ISyntaxOrSyntaxToken(nodeFieldValue)
				Else If nodeFieldValue And ..
						nodeFieldType.ExtendsType(ArrayTypeId)..' And ..
						.. '(nodeFieldType.ElementType().ExtendsType(ISyntaxOrSyntaxTokenType) Or nodeFieldType.ElementType().Interfaces().Contains(ISyntaxOrSyntaxTokenType)) .. ' reflection is too broken for this, we will make do with a null check instead
				Then
					For Local i:Int = 0 Until nodeFieldType.ArrayLength(nodeFieldValue)
						Local nodeFieldArrayElement:ISyntaxOrSyntaxToken = ISyntaxOrSyntaxToken(nodeFieldType.GetArrayElement(nodeFieldValue, i))
						If Not nodeFieldArrayElement Then Continue ' this is that null check
						VisitSyntaxInner visitor, visitMethods, , nodeFieldArrayElement
					Next
				End If
			Next
		End If
			End Rem
		
		' bottom-up visit
		For Local m:TVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = BottomUp Then
				Visit m.visitMethod, m.visitor, link, nodeOrToken
			End If
		Next
				
		Function Visit(visitMethod:TMethod, visitor:TSyntaxVisitor, link:TSyntaxLink, nodeOrToken:ISyntaxOrSyntaxToken)
			'visitMethod.Invoke visitor, [node] ' .Invoke is broken
			Local fptr(v:Object, l:Object, s:Object) = visitMethod._ref
			fptr visitor, link, nodeOrToken
		End Function
	End Function
End Function


