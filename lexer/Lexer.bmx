SuperStrict
Import "ILexer.bmx"
Import "Lexer.yy.c"	' generated by flex; see Compile Lexer.flex.bat
' a Windows build can be obtained from: https://github.com/lexxmark/winflexbison/releases (tested with 2.5.24)
' FLEX CAN NOT HANDLE UNICODE
' TODO: look into: https://www.genivia.com/reflex.html
Import BRL.LinkedList
Import BRL.FileSystem
Import BRL.Reflection



'Private ' TODO: this is broken in NG, code fails to link when extern declarations are private

Extern "C"
	Global ReportLexerError(filePath:String, line:UInt, column:UInt)
End Extern
ReportLexerError = TLexer.ReportError



Type TLexer Implements ILexer Final
	Private
	Field ReadOnly filePath:String
	Field ReadOnly tokens:TList'<TLexerToken>
	Field currentTokenLink:TLink'<TLexerToken>
	Field ReadOnly eofToken:TLexerToken ' not included in list
	
	Private
	Method New() End Method
	
	Public
	Method New(filePath:String)
		Self.filePath = filePath
		Self.tokens = ScanFile()
		Self.currentTokenLink = tokens.FirstLink()
		Self.eofToken = TLexerToken.Create(Null, TTokenKind.Eof)
	End Method
	
	Method NextToken:TLexerToken() Override
		If currentTokenLink Then
			Local token:TLexerToken = TLexerToken(currentTokenLink.Value())
			currentTokenLink = currentTokenLink.NextLink()
			Return token
		Else
			Return eofToken
		End If
	End Method
	
	Method GetPosition:ILexerPosition() Override
		Return New TLexerPosition(currentTokenLink)
	End Method
	
	Method SetPosition(position:ILexerPosition) Override
		Assert TLexerPosition(position) Else "Object is not a TLexerPosition instance"
		currentTokenLink = TLexerPosition(position).tokenLink
	End Method
	
	Private
	Method ScanFile:TList() '<TLexerToken>
		Extern "C"
			Function FlexScanFile:Int(
				filePath:String, ..
				tokenList:TList, ..
				createToken:TLexerToken(value:String, tokenKind:String, filePath:String, line:UInt, column:UInt), ..
				addToList:TLink(list:TList, token:Object) ..
			)
		End Extern
		Global tokenKindGlobals:TGlobal[] = TGlobal[](TTypeId.ForName("TTokenKind").Globals().ToArray())
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
			Return TLexerToken.Create(value, TokenKindFromName(tokenKind))
		End Function
		
		Local tokenList:TList = New TList'<TLexerToken>
		Local success:Int = FlexScanFile(filePath, tokenList, CreateToken, ListAddLast)
		If Not success Then Throw New TLexerFileReadException("Cannot read file " + filePath)
		Return tokenList
	End Method
	
	Function ReportError(filePath:String, line:UInt, column:UInt)
		' STOREAS is used on unexpected characters instead of ERROR
		' the lexer does not need any error reporting for them, they are passed to the parser as a
		' special kind of error token instead
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






