SuperStrict
Import BRL.Reflection
Import "ReflectionUtils.bmx"
Import BRL.StringBuilder
Import "SyntaxToken.bmx"
Import "SyntaxVisitor.bmx" ' TODO: untangle this, visitors should be in their own files (and folder?)



Private

Const Indent:String = "  "
Global iSyntaxType:TTypeId = TTypeId.ForName("ISyntax")
Assert iSyntaxType Else "ISyntax type not found"

? Debug
' verify correct use of {nullable}
For Local t:TTypeId = EachIn TTypeId.EnumTypes()
	If t.Interfaces() And t.Interfaces().Contains(iSyntaxType) Then
		For Local f:TField = EachIn GetAllFields(t)
			If f.MetaData("nullable") Then
				If Not f.TypeId().ExtendsType(ObjectTypeId) Then RuntimeError "Cannot use {nullable} on non-object fields (" + t.Name() + "." + f.Name() + ")"
				If f.TypeId().ExtendsType(StringTypeId) Then RuntimeError "Cannot use {nullable} on String fields (" + t.Name() + "." + f.Name() + ")"
				If f.TypeId().ExtendsType(ArrayTypeId) Then RuntimeError "Cannot use {nullable} on array fields (" + t.Name() + "." + f.Name() + ")"
			End If
		Next
	End If
Next
?



Public

Function SyntaxToString:String(syntaxOrToken:ISyntaxOrSyntaxToken, showMinorFields:Int = True)
	Local sb:TStringBuilder = New TStringBuilder
	SyntaxToString sb, syntaxOrToken, showMinorFields
	Return sb.ToString()
End Function

Function SyntaxToString(sb:TStringBuilder, syntaxOrToken:ISyntaxOrSyntaxToken, showMinorFields:Int = True)
	ToString sb, syntaxOrToken, showMinorFields, "", False
	
	Function ToString(sb:TStringBuilder, o:Object, showMinorFields:Int, indent:String, newline:Int = True)
		Local t:TTypeId = TTypeId.ForObject(o)
		If Not o Then
			Return
		Else If ISyntax(o) Then
			Local syntax:ISyntax = ISyntax(o)
			If newline Then sb.Append "~n"
			sb.Append indent
			sb.Append t.Name()
			For Local f:TField = EachIn GetAllFields(t)
				If Not showMinorFields And f.MetaData("minor") Then Continue
				sb.Append "~n"
				sb.Append indent
				sb.Append(.Indent)
				sb.Append f.Name()
				sb.Append ":"
				Local fValue:Object = f.Get(syntax)
				ToString sb, fValue, showMinorFields, indent + .Indent + .Indent
			Next
		Else If t.ExtendsType(ArrayTypeId) Then
			If t.Name() = "Null[]" Then Return
			For Local i:Int = 0 Until t.ArrayLength(o)
				ToString sb, t.GetArrayElement(o, i), showMinorFields, indent
			Next
		Else
			If newline Then sb.Append "~n"
			sb.Append indent
			sb.Append o.ToString().Replace("~n", "~n" + indent)
		End If
	End Function
End Function



Function SyntaxToCode:String(syntaxOrToken:ISyntaxOrSyntaxToken)
	Local sb:TStringBuilder = New TStringBuilder
	SyntaxToCode sb, syntaxOrToken
	Return sb.ToString()
End Function

Function SyntaxToCode(sb:TStringBuilder, syntaxOrToken:ISyntaxOrSyntaxToken)
	Local token:TSyntaxToken = TSyntaxToken(syntaxOrToken)
	If token Then
		If token.lexerToken.missing Then Return
		Function TriviaToCode(sb:TStringBuilder, lexerToken:TLexerToken[])
			For Local t:TLexerToken = EachIn lexerToken
				sb.Append t.value
			Next
		End Function
		TriviaToCode sb, token.leadingTrivia
		sb.Append token.lexerToken.value
		TriviaToCode sb, token.trailingTrivia
	Else
		Local syntax:ISyntax = ISyntax(syntaxOrToken)
		For Local child:ISyntaxOrSyntaxToken = EachIn syntax.GetChildren()
			SyntaxToCode sb, child
		Next
	End If
End Function


