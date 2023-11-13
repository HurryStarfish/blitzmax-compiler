SuperStrict
Import "../lexer/CodeRange.bmx"



' TODO:
' - make compilation unit and include directive implement a special interface that provides a file name
' - tokens will no longer store a code location
' - nodes will store the sum of the lengths of the values of all the tokens under them, excluding
'   those under the nodes that are the children of any node that implements the interface
'   (so the content of include files is not counted towards the length of the including file)
'   this will make nodes reusable after code changes
Interface ISyntaxOrSyntaxTree End Interface



' This is an implementation of red-green trees.
' The green tree consists of ISyntaxDataOrSyntaxToken objects. These objects are immutable, contain
' references to their children and are constructed during the parsing stage. When the source code is
' edited, the affected parts of the green tree are replaced.
' The red tree consists of ISyntaxOrSyntaxToken objects. ISyntax objects are internally mutable,
' but expose a public interface that presents them as immutable objects containing references to
' their parent, their children and their corresponding node or leaf from the green tree. They are
' contructed lazily when the tree is traversed by a syntax visitor. Whenever the source code is edited,
' the existing red tree is discarded and must be regenerated.
' https://ericlippert.com/2012/06/08/red-green-trees
' https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
Type TSyntaxTree Implements ISyntaxOrSyntaxTree Final
	Private
	Field ReadOnly rootSyntax:ISyntax
	
	Protected
	Method New() End Method
	
	Public
	Method New(syntaxData:ISyntaxData)
		rootSyntax = syntaxData.CreateSyntax(Self)
	End Method
	
	Method GetRootSyntax:ISyntax()
		Return rootSyntax
	End Method
End Type



Interface ISyntaxDataOrSyntaxToken
	Method CodeRange:SCodeRange()
End Interface



Interface ISyntaxOrSyntaxToken
	Method CodeRange:SCodeRange()
End Interface



Interface ISyntaxData Extends ISyntaxDataOrSyntaxToken
	' classes implementing this interface should have their fields written in the same order as
	' the corresponding tokens would appear in code and must be able to contain at least one
	' TSyntaxToken (directly or indirectly in one of its fields);
	' fields that can contain null objects (this does not include arrays, strings, ...) should be
	' marked as {nullable}, fields that are not semantically relevant should be marked as {minor}
	
	Method CreateSyntax:ISyntax(parent:ISyntaxOrSyntaxTree) ' this is here instead of in TSyntax constructor so that overridden methods can be used to create a TSyntax of the correct type from a TSyntaxData without knowing the type statically
	
	Method GetChildren:ISyntaxDataOrSyntaxToken[]()
End Interface



Interface ISyntax Extends ISyntaxOrSyntaxToken, ISyntaxOrSyntaxTree
	Method Data:ISyntaxData()
	Method GetSyntaxTree:TSyntaxTree()
	Method Parent:ISyntax()
	Method GetChildren:ISyntaxOrSyntaxToken[]()
End Interface



Type TSyntaxData Implements ISyntaxData Abstract
	Protected
	Method New() End Method
	
	Public
	Method CodeRange:SCodeRange() Override Final
		Local children:ISyntaxDataOrSyntaxToken[] = GetChildren()
		If children.length = 0 Then
			Return Null
		Else
			If children.length = 1 Then
				Return children[0].CodeRange()
			Else
				' return combined range of children
				Local firstValidChildRange:SCodeRange
				Local lastValidChildRange:SCodeRange
				Local c:Int
				For c = 0 To children.length - 1
					Local r:SCodeRange = children[c].CodeRange()
					If r.IsValid() Then firstValidChildRange = r; Exit
				Next
				If c = children.length Then
					Return Null ' no child with a valid range
				Else
					For c = children.length - 1 To c Step -1
						Local r:SCodeRange = children[c].CodeRange()
						If r.IsValid() Then lastValidChildRange = r; Exit
					Next
					Return New SCodeRange(firstValidChildRange.startLocation, lastValidChildRange.endLocation)
				End If
			End If
		End If
	End Method
	
	' TODO: ToString
	
	' TODO: ToCode
End Type



Type TSyntax Implements ISyntax Abstract
	Protected
	Field ReadOnly _parent:ISyntaxOrSyntaxTree ' nullable
	
	Method New() End Method
	
	Public
	Method GetSyntaxTree:TSyntaxTree() Override Final
		Local syntax:TSyntax = Self
		While TSyntax(syntax._parent) syntax = TSyntax(syntax._parent) Wend
		Assert TSyntaxTree(syntax._parent) Else "Failed to get syntax tree"
		Return TSyntaxTree(syntax._parent)
	End Method
	
	Method Parent:ISyntax() Override Final ' nullable
		Return ISyntax(_parent)
	End Method
	
	Method CodeRange:SCodeRange() Override Final
		Return Data().CodeRange()
	End Method
End Type

