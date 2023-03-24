SuperStrict
Import BRL.Reflection
Import "ReflectionUtils.bmx"
Import BRL.StringBuilder
Import "SyntaxTree.bmx"



Private
Const Indent:String = "  "

Public
Function SyntaxToString:String(link:TSyntaxLink, syntaxOrToken:ISyntaxOrSyntaxToken, showMinorFields:Int = True, additionalData:TMap = Null)'<ISyntax|TSyntaxLink, Object>
	Assert link.GetSyntaxOrSyntaxToken() = syntaxOrToken Else "Link does not match syntax or syntax token"
	Local sb:TStringBuilder = New TStringBuilder
	SyntaxToString sb, link, syntaxOrToken, showMinorFields, additionalData
	Return sb.ToString()
End Function

Function SyntaxToString(sb:TStringBuilder, link:TSyntaxLink, syntaxOrToken:ISyntaxOrSyntaxToken, showMinorFields:Int = True, additionalData:TMap = Null)
	ToString sb, link, syntaxOrToken, showMinorFields, "", False, additionalData
	
	Function ToString(sb:TStringBuilder, link:TSyntaxLink, o:Object, showMinorFields:Int, indent:String, newline:Int, additionalData:TMap)
		Local t:TTypeId = TTypeId.ForObject(o)
		If Not o Then
			Return
		Else If ISyntax(o) Then
			Local syntax:ISyntax = ISyntax(o)
			If newline Then sb.Append "~n"
			sb.Append indent
			sb.Append t.Name()
			If additionalData Then
				Local d:Object = additionalData.ValueForKey(link)
				If d Then sb.Append " --- "; sb.Append d.ToString()
			End If
			For Local f:TField = EachIn GetAllFields(t)
				If Not showMinorFields And f.MetaData("minor") Then Continue
				sb.Append "~n"
				sb.Append indent
				sb.Append(.Indent)
				sb.Append f.Name()
				sb.Append ":"
				Local fValue:Object = f.Get(syntax)
				If fValue And TTypeId.ForObject(fValue).ExtendsType(ArrayTypeId) Then ' TODO
					ToString sb, link, fValue, showMinorFields, indent + .Indent + .Indent, True, additionalData
				Else If fValue Then
					Assert ISyntaxOrSyntaxToken(fValue) Else "Object is not syntax or syntax token"
					ToString sb, link.FindChild(ISyntaxOrSyntaxToken(fValue)), fValue, showMinorFields, indent + .Indent + .Indent, True, additionalData
				End If
			Next
		Else If t.ExtendsType(ArrayTypeId) Then
			If t.Name() = "Null[]" Then Return
			For Local i:Int = 0 Until t.ArrayLength(o)
				Local element:Object = t.GetArrayElement(o, i)
				Assert ISyntaxOrSyntaxToken(element) Else "Object is not syntax or syntax token"
				ToString sb, link.FindChild(ISyntaxOrSyntaxToken(element)), element, showMinorFields, indent, True, additionalData
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


