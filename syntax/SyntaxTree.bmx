SuperStrict
Import "Syntax.bmx"
Import "../util/WeakReference.bmx"
Include "SyntaxLink.bmx"



' This is an implementation of red-green trees.
' The green tree consists of TSyntaxOrSyntaxToken objects. These objects are immutable and contain
' references to their respective children. They are constructed during the parsing stage. When the
' source code is edited, the affected parts of the green tree are replaced.
' The red tree consists of TSyntaxLink objects. These objects are internally mutable, but present
' themselves as immutable, containing references to their respective parent, children and corresponding
' node or leaf from the green tree. They are contructed lazily when the tree is traversed by a syntax
' visitor. When the source code is edited, the entire existing red tree is discarded.
' https://ericlippert.com/2012/06/08/red-green-trees
' https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
Type TSyntaxTree Final
	Private
	Field ReadOnly root:TSyntaxLink
	
	Private
	Method New() End Method

	Public
	Method New(syntax:ISyntax)
		root = TSyntaxLink.CreateRoot(Self, syntax)
	End Method
	
	Method GetRoot:TSyntaxLink()
		Return root
	End Method
End Type
