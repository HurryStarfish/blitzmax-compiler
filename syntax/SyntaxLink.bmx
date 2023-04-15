'SuperStrict
'Import "Syntax.bmx"
'Import "../util/WeakReference.bmx"



Type TSyntaxLink Final
	Private
	Field ReadOnly parent:Object 'TSyntaxLink | TSyntaxTree
	Field children:Object 'TSyntaxLink[] | TWeakReference'<TSyntaxLink[]>
	Field ReadOnly syntaxOrSyntaxToken:ISyntaxOrSyntaxToken
	Field ReadOnly codeRange:SCodeRange
	
	Private
	Method New() End Method
	
	Method New(syntaxTree:TSyntaxTree, syntaxOrSyntaxToken:ISyntaxOrSyntaxToken) ' for the root node only
		Self.parent = syntaxTree
		Self.syntaxOrSyntaxToken = syntaxOrSyntaxToken
	End Method
	
	Public
	Method New(parent:TSyntaxLink, syntaxOrSyntaxToken:ISyntaxOrSyntaxToken)
		Self.parent = parent
		Self.syntaxOrSyntaxToken = syntaxOrSyntaxToken
	End Method
	
	Function CreateRoot:TSyntaxLink(syntaxTree:TSyntaxTree, syntaxOrSyntaxToken:ISyntaxOrSyntaxToken)
		Return New TSyntaxLink(syntaxTree, syntaxOrSyntaxToken)
	End Function
	
	Method GetSyntaxTree:TSyntaxTree()
		Local link:TSyntaxLink = Self
		While TSyntaxLink(link.parent) link = TSyntaxLink(link.parent) Wend
		Return TSyntaxTree(link.parent)
	End Method
	
	Method GetParent:TSyntaxLink() 'nullable
		Return TSyntaxLink(parent)
	End Method
	
	Method GetParentSyntax:ISyntax() 'nullable
		Local parent:TSyntaxLink = GetParent()
		If parent Then Return ISyntax(parent.syntaxOrSyntaxToken) Else Return Null
	End Method
	
	Method GetChildren:TSyntaxLink[]()
		If TCodeBlockSyntax(syntaxOrSyntaxToken) Then
			' links for code blocks are weakly referenced, so that the corresponding
			' subtree can be reclaimed by GC when it is no longer referenced anywhere else
			Local children:TSyntaxLink[]
			If TWeakReference(Self.children) Then children = TSyntaxLink[](TWeakReference(Self.children).Get())
			If Not children Then
				children = CreateChildren(Self)
				Self.children = New TWeakReference(children)
			End If
			Return children
		Else
			If Not Self.children And ISyntax(syntaxOrSyntaxToken) Then
				Self.children = CreateChildren(Self)
			End If
			Return TSyntaxLink[](Self.children)
		End If
		
		Function CreateChildren:TSyntaxLink[](self_:TSyntaxLink)
			Local syntaxChildren:ISyntaxOrSyntaxToken[] = ISyntax(self_.syntaxOrSyntaxToken).GetChildren()
			Local children:TSyntaxLink[syntaxChildren.length]
			For Local c:Int = 0 Until children.length
				children[c] = New TSyntaxLink(self_, syntaxChildren[c])
			Next
			Return children
		End Function
	End Method
	
	Method FindChild:TSyntaxLink(childSyntaxOrSyntaxToken:ISyntaxOrSyntaxToken) ' must be a direct child
		' TODO: this will not work with reused syntax nodes!
		For Local child:TSyntaxLink = EachIn GetChildren()
			If child.syntaxOrSyntaxToken = childSyntaxOrSyntaxToken Then Return child
		Next
		RuntimeError "Child link not found"
	End Method
	
	Method GetSyntaxOrSyntaxToken:ISyntaxOrSyntaxToken()
		Return syntaxOrSyntaxToken
	End Method
	
	Method GetCodeRange:SCodeRange()
		Return syntaxOrSyntaxToken.CodeRange()
	End Method
End Type
