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

Global VisitMethods:TMap = New TMap'<TTypeId, TVisitMethodAndDirection[]>
'VerifyAndIndexVisitorTypes



Enum EVisitDirection:Byte
	TopDown
	BottomUp
End Enum

Struct SVisitMethodAndDirection
	Field ReadOnly visitMethod:TMethod
	Field ReadOnly direction:EVisitDirection
	
	Method New(visitMethod:TMethod, direction:EVisitDirection)
		Self.visitMethod = visitMethod
		Self.direction = direction
	End Method
End Struct

Struct SVisitMethodAndDirectionAndVisitor
	Field ReadOnly visitMethod:TMethod
	Field ReadOnly direction:EVisitDirection
	Field ReadOnly visitor:TSyntaxVisitor
	
	Method New(visitMethod:TMethod, direction:EVisitDirection, visitor:TSyntaxVisitor)
		Self.visitMethod = visitMethod
		Self.direction = direction
		Self.visitor = visitor
	End Method
End Struct



Rem
Function VerifyAndIndexVisitorTypes()
	VerifyAndIndexSubtypes TTypeId.ForName("TSyntaxVisitor")
	Function VerifyAndIndexSubtypes(superType:TTypeId)
		Local d:Object= superType.DerivedTypes().ToArray()
		For Local t:TTypeId = EachIn superType.DerivedTypes() ' BROKEN: DerivedTypes() is empty
			VisitMethods[t] = GetVisitMethods(t)
			VerifyAndIndexSubtypes t
		Next
	End Function
End Function
End Rem
Function GetVisitMethods:SVisitMethodAndDirection[](visitorType:TTypeId)
	Local visitMethods:SVisitMethodAndDirection[]
	' TODO: BROKEN! this may randomly skip some of the methods because reflection is broken; this
	' seems to be random(!) per build (different results when the same code is compiled multiple times)
	For Local m:TMethod = EachIn visitorType._methodsList
		If m.Name() = "VisitTopDown" Or m.Name() = "VisitBottomUp" Then
			Local direction:EVisitDirection
			Select m.Name()
				Case "VisitTopDown" direction = EVisitDirection.TopDown
				Case "VisitBottomUp" direction = EVisitDirection.BottomUp
			End Select
			Local returnType:TTypeId = m.TypeId().ReturnType()
			Local argTypes:TTypeId[] = m.ArgTypes()
			Assert returnType = VoidTypeId Else "Invalid return type on " + visitorType.Name() + "." + m.Name()
			Assert argTypes.length = 2 Else "Invalid parameter count on " + visitorType.Name() + "." + m.Name()
			Assert argTypes[0].ExtendsType(TSyntaxLinkType) Else "Invalid 1st parameter type on " + visitorType.Name() + "." + m.Name() + " (must be TSyntaxLink)"
			Assert argTypes[1] = ISyntaxType Or Not argTypes[0].IsInterface() Else "Invalid 2nd parameter type on " + visitorType.Name() + "." + m.Name() + " (must be a subtype of ISyntaxOrSyntaxToken, can't use interfaces other than ISyntax due to reflection being broken)"
			Assert argTypes[1].ExtendsType(ISyntaxType) Or argTypes[1].Interfaces().Contains(ISyntaxType) Or ..
				   argTypes[1].ExtendsType(TSyntaxTokenType) Else "Invalid 2nd parameter type on " + visitorType.Name() + "." + m.Name()
			visitMethods :+ [New SVisitMethodAndDirection(m, direction)]
		End If
	Next
	Assert visitMethods Else "No VisitTopDown or VisitBottomUp method found in " + visitorType.Name()
	Return visitMethods
End Function



Function VisitSyntax(visitor:TSyntaxVisitor, rootLink:TSyntaxLink)
	Local map:TMap = VisitMethods ' because ".VisitMethods" is broken
	Local visitorType:TTypeId = TTypeId.ForObject(visitor)
	Local visitMethods:SVisitMethodAndDirection[] = SVisitMethodAndDirection[](map[visitorType])
	If Not visitMethods Then
		visitMethods = GetVisitMethods(visitorType)
		map[visitorType] = visitMethods
	End If
	
	VisitSyntaxInner visitor, visitMethods, rootLink, rootLink.GetSyntaxOrSyntaxToken()
	
	Function VisitSyntaxInner(visitor:TSyntaxVisitor, visitMethods:SVisitMethodAndDirection[], link:TSyntaxLink, nodeOrToken:ISyntaxOrSyntaxToken)		
		' select visit methods applicable to this node
		Local nodeOrTokenType:TTypeId = TTypeId.ForObject(nodeOrToken)
		Local applicableMethods:SVisitMethodAndDirectionAndVisitor[]
		For Local v:SVisitMethodAndDirection = EachIn visitMethods
			Local syntaxArgType:TTypeId = v.visitMethod.ArgTypes()[1]
			If nodeOrTokenType.ExtendsType(syntaxArgType) Or nodeOrTokenType.Interfaces().Contains(syntaxArgType) Then ' this is so broken
				applicableMethods :+ [New SVisitMethodAndDirectionAndVisitor(v.visitMethod, v.direction, visitor)]
			End If
		Next
		
		' top-down visit
		For Local m:SVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = EVisitDirection.TopDown Then
				Visit m.visitMethod, m.visitor, link, nodeOrToken
			End If
		Next
		
		For Local childLink:TSyntaxLink = EachIn link.GetChildren()
			VisitSyntaxInner visitor, visitMethods, childLink, childLink.GetSyntaxOrSyntaxToken()
		Next
		
		' bottom-up visit
		For Local m:SVisitMethodAndDirectionAndVisitor = EachIn applicableMethods
			If m.direction = EVisitDirection.BottomUp Then
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


