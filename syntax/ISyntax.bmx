SuperStrict
Import "SyntaxOrSyntaxToken.bmx"



Interface ISyntax Extends ISyntaxOrSyntaxToken
	' classes implementing this interface should have their fields written in the same order as
	' the corresponding tokens would appear in code and must have at least one field of type TSyntaxToken;
	' fields that can contain null objects (this does not include arrays, strings, ...) should be
	' marked as {nullable}, fields that are not semantically relevant should be marked as {minor}
	
	Method GetChildren:ISyntaxOrSyntaxToken[]()
End Interface
