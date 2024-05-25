SuperStrict
Import BRL.Reflection
Import "../util/ReflectionUtils.bmx"
Import BRL.StringBuilder
Import "SyntaxBase.bmx"
Import "SyntaxToken.bmx"
Import "../util/WeakReference.bmx"
Import "SyntaxVisitor.bmx"



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
			If t = ArrayTypeId Then Return ' null array
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

Rem
Type TSyntaxToCodeVisitor Extends TSyntaxVisitor Final
	Field stringBuilder:TStringBuilder
	
	Private
	Field rootCodeFileTopLevelSyntax:TCodeFileTopLevelSyntax
	
	Method VisitTopDown:TCodeFileTopLevelSyntax(codeFileTopLevelSyntax:TCodeFileTopLevelSyntax, parentCodeFileTopLevelSyntax:TCodeFileTopLevelSyntax)
		If Not rootCodeFileTopLevelSyntax Then
			Assert Not stringBuilder Else "Multiple top-level nodes have been visited"
			stringBuilder = New TStringBuilder(codeFileTopLevelSyntax.CodeInfo().length)
			rootCodeFileTopLevelSyntax = codeFileTopLevelSyntax
		End If
		Assert "Builder was not created when top-level node was visited"
		Return codeFileTopLevelSyntax
	End Method
	
	Method VisitTopDown:TCodeFileTopLevelSyntax(token:TSyntaxToken, parentCodeFileTopLevelSyntax:TCodeFileTopLevelSyntax)
		Assert rootCodeFileTopLevelSyntax Else "Top-level node was not yet visited"
		If parentCodeFileTopLevelSyntax = rootCodeFileTopLevelSyntax Then ' skip code from included files
			If Not token.lexerToken.missing Then
				Function TriviaToCode(sb:TStringBuilder, lexerToken:TLexerToken[])
					For Local t:TLexerToken = EachIn lexerToken
						sb.Append t.value
					Next
				End Function
				TriviaToCode stringBuilder, token.leadingTrivia
				stringBuilder.Append token.lexerToken.value
				TriviaToCode stringBuilder, token.trailingTrivia
			End If
			Return parentCodeFileTopLevelSyntax
		End If
	End Method
End Type
End Rem

Type TSyntaxToCodeVisitor Extends TSyntaxVisitor Final
	Field fileResults:TSyntaxToCodeVisitorFileResult[]
	
	Method VisitTopDown:TSyntaxToCodeVisitorFileResult(codeFileTopLevelSyntax:TCodeFileTopLevelSyntax, parentFileResult:TSyntaxToCodeVisitorFileResult)
		Local r:TSyntaxToCodeVisitorFileResult = New TSyntaxToCodeVisitorFileResult(codeFileTopLevelSyntax.CodeFilePath(), New TStringBuilder(codeFileTopLevelSyntax.CodeInfo().length))
		fileResults :+ [r]
		Return r
	End Method
	
	Method VisitTopDown:TSyntaxToCodeVisitorFileResult(syntaxToken:TSyntaxToken, fileResult:TSyntaxToCodeVisitorFileResult)
		TokenToCode syntaxToken, fileResult.stringBuilder
		Return fileResult
	End Method
	
	Function TokenToCode(syntaxToken:TSyntaxToken, sb:TStringBuilder)
		For Local t:TLexerToken = EachIn syntaxToken.leadingTrivia
			If Not t.missing Then sb.Append t.value
		Next
		If Not syntaxToken.lexerToken.missing Then sb.Append syntaxToken.lexerToken.value
		For Local t:TLexerToken = EachIn syntaxToken.trailingTrivia
			If Not t.missing Then sb.Append t.value
		Next
	End Function
End Type

Type TSyntaxToCodeVisitorFileResult Final
	Field ReadOnly codeFilePath:String
	Field ReadOnly stringBuilder:TStringBuilder
	
	Method New(codeFilePath:String, stringBuilder:TStringBuilder)
		Self.codeFilePath = codeFilePath
		Self.stringBuilder = stringBuilder
	End Method
End Type











