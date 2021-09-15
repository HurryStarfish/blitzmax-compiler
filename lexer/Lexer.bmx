SuperStrict
Import "ILexer.bmx"
Import "Lexer.yy.c"	' generated by flex; see Compile Lexer.flex.bat ' TODO: look into https://www.genivia.com/reflex.html
Import BRL.LinkedList
Import BRL.FileSystem
Import BRL.Reflection



'Private ' TODO: this is broken in NG, code fails to link when extern declarations are private

Extern "C"
	Global ReportLexerError(filePath:String, line:UInt, column:UInt)
End Extern
ReportLexerError = TLexer.ReportError



Type TLexer Implements ILexer
	Private
	Field tokens:TList'<TLexerToken>
	Field tokenLink:TLink'<TLexerToken>
	
	Private
	Method New() End Method
	
	Public
	Method New(filePath:String)
		Self.tokens = ScanFile(filePath)
		Self.tokenLink = tokens.FirstLink()
	End Method

	Method NextToken:TLexerToken() Override
		If tokenLink Then
			Local token:TLexerToken = TLexerToken(tokenLink.Value())
			tokenLink = tokenLink.NextLink()
			Return token
		Else
			Return EOFToken
		End If
	End Method
	
	Method GetPosition:ILexerPosition() Override
		Return New TLexerPosition(tokenLink)
	End Method
	
	Method SetPosition(position:ILexerPosition) Override
		Assert TLexerPosition(position) Else "Object is not a TLexerPosition instance"
		tokenLink = TLexerPosition(position).tokenLink
	End Method
	
	Method ScanFile:TList(filePath:String) '<TLexerToken>
		Extern "C"
			Function FlexScanFile:Int(
				filePath:String, ..
				tokenList:TList, ..
				createToken:TLexerToken(value:String, tokenKind:String, filePath:String, line:UInt, column:UInt), ..
				addToList:TLink(list:TList, token:Object) ..
			)
		End Extern
		Global tokenKindGlobals:TList = TTypeId.ForName("TTokenKind")._globalsList
		Function TokenKindFromName:TTokenKind(name:String)
			For Local g:TGlobal = EachIn tokenKindGlobals
				Local kind:TTokenKind = TTokenKind(g.Get())
				If kind.name = name Then Return kind
			Next
			RuntimeError "Token kind " + name + " does not exist"
		End Function
		
		Function ProcessFilename:String(filename:String)
			Return StripDir(filename) + " [" + filename + "]"
		End Function
		
		Function CreateToken:TLexerToken(value:String, tokenKind:String, file:String, line:UInt, column:UInt)
			Return New TLexerToken(value, TokenKindFromName(tokenKind), New SCodeLocation(file, line, column))
		End Function
		
		Local tokenList:TList = New TList'<TLexerToken>
		Local success:Int = FlexScanFile(filePath, tokenList, CreateToken, ListAddLast)
		If Not success Then Throw "Cannot read file " + filePath ' TODO: proper exception
		Return tokenList
	End Method
	
	Function ReportError(filePath:String, line:UInt, column:UInt)
		' TODO: proper error reporting with TLexError
		'       gather errors for later reporting instead of throwing
		'       use STOREAS on unexpected characters instead of ERROR so that they are not lost
		'       the lexer might not even need any error reporting then (beyond exceptions thrown by
		'       ScanFile for unreadable files or some such) - the parser can do that
		Throw "Lexer Error in file " + filePath + " at l:" + line + ", c:" + column
	End Function
End Type



Private

Type TLexerPosition Implements ILexerPosition Final
	Field ReadOnly tokenLink:TLink
	
	Private
	Method New() End Method
	
	Public
	Method New(tokenLink:TLink)
		Self.tokenLink = tokenLink
	End Method
End Type






