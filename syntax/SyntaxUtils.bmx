SuperStrict
Import BRL.Reflection
Import "../util/ReflectionUtils.bmx"
Import BRL.StringBuilder
Import "SyntaxBase.bmx"
Import "SyntaxToken.bmx"
Import "../util/WeakReference.bmx"



Private
Const Indent:String = "  "

Public
Function SyntaxToString:String(syntaxOrToken:ISyntaxOrSyntaxToken, showMinorFields:Int = True, additionalData:TMap = Null)'<ISyntaxData|ISyntaxOrSyntaxToken, Object>
	Local sb:TStringBuilder = New TStringBuilder
	SyntaxToString sb, syntaxOrToken, showMinorFields, additionalData
	Return sb.ToString()
End Function

Function SyntaxToString(sb:TStringBuilder, syntaxOrToken:ISyntaxOrSyntaxToken, showMinorFields:Int = True, additionalData:TMap = Null)
	ToString sb, syntaxOrToken, showMinorFields, "", False, additionalData
	
	Function ToString(sb:TStringBuilder, o:Object, showMinorFields:Int, indent:String, newline:Int, additionalData:TMap)
		If Not o Then Return
		Local t:TTypeId = TTypeId.ForObject(o)
		If TWeakReference(o) Then
			ToString sb, TWeakReference(o).Get(), showMinorFields, indent, newline, additionalData
		Else If ISyntax(o) Then
			Local syntax:ISyntax = ISyntax(o)
			Local dt:TTypeId = TTypeId.ForObject(syntax.Data())
			If newline Then sb.Append "~n"
			sb.Append indent
			sb.Append t.Name()
			If additionalData Then
				Local d:Object = additionalData.ValueForKey(o)
				If d Then sb.Append " --- "; sb.Append d.ToString()
			End If
			For Local f:TField = EachIn GetAllFields(dt)
				If Not showMinorFields And f.MetaData("minor") Then Continue
				Local m:TMethod = t.FindMethod(f.Name())
				sb.Append "~n"
				sb.Append indent
				sb.Append(.Indent)
				sb.Append m.Name()
				sb.Append ":"
				Local mValue:Object = m.Invoke(syntax)
				If mValue And TTypeId.ForObject(mValue).ExtendsType(ArrayTypeId) Then ' TODO
					ToString sb, mValue, showMinorFields, indent + .Indent + .Indent, True, additionalData
				Else If mValue Then
					'Assert ISyntaxOrSyntaxToken(mValue) Else "Object is not syntax or syntax token"
					ToString sb, mValue, showMinorFields, indent + .Indent + .Indent, True, additionalData
				End If
			Next
		Else If t.ExtendsType(ArrayTypeId) Then
			If t.Name() = "Null[]" Then Return
			For Local i:Int = 0 Until t.ArrayLength(o)
				Local element:Object = t.GetArrayElement(o, i)
				'Assert ISyntaxOrSyntaxToken(element) Else "Object is not syntax or syntax token"
				ToString sb, element, showMinorFields, indent, True, additionalData
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


