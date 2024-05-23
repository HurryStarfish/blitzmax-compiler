SuperStrict
Import "../lexer/CodeRange.bmx"
Import "../lexer/CodeInfo.bmx"



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
	' see ISyntaxOrSyntaxToken
	Method CodeInfo:SCodeInfo()
End Interface



Interface ISyntaxOrSyntaxToken
	' returns information about the code string represented by this node and all of its children
	' within the same file
	' if the subtree below this node contains an Include directive, the code of the directive itself
	' is covered by the returned info, but the code inside the included file is not
	Method CodeInfo:SCodeInfo()
End Interface



Interface ISyntaxData Extends ISyntaxDataOrSyntaxToken
	' classes implementing this interface should have their fields written in the same order as
	' the corresponding tokens would appear in code and must be able to contain at least one
	' TSyntaxToken (directly or indirectly in one of its fields);
	' fields that can contain null objects (this does not include arrays, strings, ...) should be
	' marked as {nullable}, fields that are not semantically relevant should be marked as {minor}
	
	Method CreateSyntax:ISyntax(parent:ISyntaxOrSyntaxTree) ' this is here instead of in TSyntax descendants so that it can be overridden to create *Syntax instances of the correct type from *SyntaxData instances without knowing the type of the instances statically
	
	Method GetChildren:ISyntaxDataOrSyntaxToken[]()
End Interface



Interface ISyntax Extends ISyntaxOrSyntaxToken, ISyntaxOrSyntaxTree
	Method Data:ISyntaxData()
	Method GetSyntaxTree:TSyntaxTree()
	Method Parent:ISyntax()
	Method GetChildren:ISyntaxOrSyntaxToken[]()
	Method CodeRange:SCodeRange()
	Method ChildCodeRange:SCodeRange(child:ISyntaxOrSyntaxToken)
End Interface



Type TSyntaxData Implements ISyntaxData Abstract
	Protected
	Method New() End Method
	
	Public
	Method CodeInfo:SCodeInfo() Override Final
		Local children:ISyntaxDataOrSyntaxToken[] = GetChildren()
		Local codeInfo:SCodeInfo = New SCodeInfo()
		For Local child:ISyntaxDataOrSyntaxToken = EachIn children
			If TCodeFileTopLevelSyntaxData(child) Then
				' code is in a different file, so it is not considered
			Else
				codeInfo = codeInfo + child.CodeInfo()
			End If
		Next
		Return codeInfo
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
	
	Method Parent:TSyntax() Override Final ' nullable
		Return TSyntax(_parent)
	End Method
		
	Method CodeInfo:SCodeInfo() Override Final
		Return Data().CodeInfo()
	End Method
	
	Method CodeRange:SCodeRange() Override Final
		'TODO: provide a way to get the range without the trivia
		
		Local precedingCodeInfo:SCodeInfo = GetPrecedingCodeInfoInFile()
		Local codeInfo:SCodeInfo = CodeInfo()
		Local codeFilePath:String = GetCodeFilePath()
		
		Local startCodeInfo:SCodeInfo = precedingCodeInfo
		Local endCodeInfo:SCodeInfo = precedingCodeInfo + codeInfo
		Local startLocation:SCodeLocation = New SCodeLocation(codeFilePath, startCodeInfo.linebreakCount + 1, startCodeInfo.charsAfterLastLinebreak + 1)
		Local endLocation:SCodeLocation = New SCodeLocation(codeFilePath, endCodeInfo.linebreakCount + 1, endCodeInfo.charsAfterLastLinebreak + 1)
		Return New SCodeRange(startLocation, endLocation)
	End Method
	
	Method ChildCodeRange:SCodeRange(child:ISyntaxOrSyntaxToken) Override Final
		' basically the same as CodeRange(), but can be used for tokens too
				
		Local precedingCodeInfo:SCodeInfo = GetChildPrecedingCodeInfoInFile(child)
		Local codeInfo:SCodeInfo = child.CodeInfo()
		Local codeFilePath:String = GetCodeFilePath() ' TODO: this should be the child's
		
		Local startCodeInfo:SCodeInfo = precedingCodeInfo
		Local endCodeInfo:SCodeInfo = precedingCodeInfo + codeInfo
		Local startLocation:SCodeLocation = New SCodeLocation(codeFilePath, startCodeInfo.linebreakCount + 1, startCodeInfo.charsAfterLastLinebreak + 1)
		Local endLocation:SCodeLocation = New SCodeLocation(codeFilePath, endCodeInfo.linebreakCount + 1, endCodeInfo.charsAfterLastLinebreak + 1)
		Return New SCodeRange(startLocation, endLocation)
	End Method
	
	Private
	Method GetPrecedingCodeInfoInFile:SCodeInfo() Final
		If TCodeFileTopLevelSyntax(Self) Then
			Return New SCodeInfo()
		Else
			Assert Parent() Else "Syntax is a not a file top-level syntax but has no parent"
			Return Parent().GetChildPrecedingCodeInfoInFile(Self)
		End If
	End Method
	
	Method GetChildPrecedingCodeInfoInFile:SCodeInfo(child:ISyntaxOrSyntaxToken) Final
		Local codeInfo:SCodeInfo
		If TCodeFileTopLevelSyntax(Self) Then
			codeInfo = New SCodeInfo()
		Else
			Assert Parent() Else "Syntax is a not a file top-level syntax but has no parent"
			codeInfo = Parent().GetChildPrecedingCodeInfoInFile(Self)
		End If
		codeInfo = codeInfo + GetChildPrecedingCodeInfoInNode(child)
		Return codeInfo
	End Method
	
	Method GetChildPrecedingCodeInfoInNode:SCodeInfo(child:ISyntaxOrSyntaxToken) Final
		Local codeInfo:SCodeInfo = New SCodeInfo()
		For Local c:ISyntaxOrSyntaxToken = EachIn GetChildren()
			If c = child Then Return codeInfo Else codeInfo = codeInfo + c.CodeInfo()
		Next
		RuntimeError "Child not found"
	End Method
	
	Method GetCodeFilePath:String() Final
		Local selfAsTopLevelSyntax:TCodeFileTopLevelSyntax = TCodeFileTopLevelSyntax(Self)
		If selfAsTopLevelSyntax Then
			Return selfAsTopLevelSyntax.CodeFilePath()
		Else
			Assert Parent() Else "Syntax is a not a file top-level syntax but has no parent"
			Return Parent().GetCodeFilePath()
		End If
	End Method
End Type



Type TCodeFileTopLevelSyntaxData Extends TSyntaxData Abstract
End Type

Type TCodeFileTopLevelSyntax Extends TSyntax Abstract
	Method CodeFilePath:String() Abstract
End Type

