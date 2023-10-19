SuperStrict
Import "ParseError.bmx"
Import "../syntax/Syntax.bmx"
Import BRL.LinkedList



Interface IParser
	Method ParseCompilationUnit:TCompilationUnitSyntaxData()
	Method Errors:TList()'<TParseError>
End Interface

