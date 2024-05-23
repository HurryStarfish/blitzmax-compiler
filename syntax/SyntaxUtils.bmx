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
Enum ESyntaxToStringOptions Flags
	ShowMinorFields
	ShowCodeRanges
End Enum

Function SyntaxToString:String(syntaxOrToken:ISyntaxOrSyntaxToken, options:ESyntaxToStringOptions = Null, additionalData:TMap = Null)'<ISyntaxData|ISyntaxOrSyntaxToken, Object>
	Local sb:TStringBuilder = New TStringBuilder
	SyntaxToString sb, syntaxOrToken, options, additionalData
	Return sb.ToString()
End Function

Function SyntaxToString(sb:TStringBuilder, syntaxOrToken:ISyntaxOrSyntaxToken, options:ESyntaxToStringOptions = Null, additionalData:TMap = Null)
	ToString sb, syntaxOrToken, options, additionalData, "", False, Null
	
	Function ToString(sb:TStringBuilder, o:Object, options:ESyntaxToStringOptions, additionalData:TMap, indent:String, newline:Int, parent:ISyntax)
		If Not o Then Return
		Local t:TTypeId = TTypeId.ForObject(o)
		If TWeakReference(o) Then
			ToString sb, TWeakReference(o).Get(), options, additionalData, indent, newline, parent
		Else If ISyntaxOrSyntaxToken(o) Then
			Local syntaxOrSyntaxToken:ISyntaxOrSyntaxToken = ISyntaxOrSyntaxToken(o)
			Local syntax:ISyntax = ISyntax(o)
			Local syntaxToken:TSyntaxToken = TSyntaxToken(o)
			Assert syntax Or syntaxToken Else "Missing case"
			
			If newline Then sb.Append "~n"
			sb.Append indent
			If syntax Then sb.Append t.Name() Else sb.Append o.ToString().Replace("~n", "~n" + indent)
			
			If options & ESyntaxToStringOptions.ShowCodeRanges Then
				sb.Append " ("
				If syntax Then
					sb.Append syntax.CodeRange().ToString()
				Else
					sb.Append parent.ChildCodeRange(syntaxToken).ToString()
				End If
				sb.Append ")"
			End If
			
			If additionalData Then
				Local d:Object = additionalData.ValueForKey(o)
				If d Then sb.Append " --- "; sb.Append d.ToString()
			End If
			
			If syntax Then
				Local dt:TTypeId = TTypeId.ForObject(syntax.Data())
				For Local f:TField = EachIn dt.EnumFields()
					If Not (options & ESyntaxToStringOptions.ShowMinorFields) And f.MetaData("minor") Then Continue
					Local m:TMethod = t.FindMethod(f.Name())
					sb.Append "~n"
					sb.Append indent
					sb.Append(.Indent)
					sb.Append m.Name()
					sb.Append ":"
					Local mValue:Object = m.Invoke(syntax)
					If mValue And TTypeId.ForObject(mValue).ExtendsType(ArrayTypeId) Then ' TODO
						ToString sb, mValue, options, additionalData, indent + .Indent + .Indent, True, syntax
					Else If mValue Then
						'Assert ISyntaxOrSyntaxToken(mValue) Else "Object is not syntax or syntax token"
						ToString sb, mValue, options, additionalData, indent + .Indent + .Indent, True, syntax
					End If
				Next
			End If
		Else If t.ExtendsType(ArrayTypeId) Then
			If t.Name() = "Null[]" Then Return
			For Local i:Int = 0 Until t.ArrayLength(o)
				Local element:Object = t.GetArrayElement(o, i)
				'Assert ISyntaxOrSyntaxToken(element) Else "Object is not syntax or syntax token"
				ToString sb, element, options, additionalData, indent, True, parent
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


